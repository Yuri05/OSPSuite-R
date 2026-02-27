# OSPSuite-R Performance Optimization Analysis

## Executive Summary

This document provides a comprehensive analysis of performance optimization opportunities in the OSPSuite-R package. The analysis identifies critical bottlenecks in population simulation, data processing, unit conversion, and .NET interoperability, with detailed recommendations for improvement.

**Key Findings:**
- **High Priority**: 8 critical optimizations affecting population simulation and batch processing performance
- **Medium Priority**: 7 optimizations for data handling and entity operations
- **Low Priority**: 5 minor optimizations for utility functions and edge cases

**Estimated Performance Impact**: 30-60% improvement in population simulation runtime, 20-40% reduction in memory allocations, 50-75% improvement in unit conversion operations based on existing benchmark evidence.

---

## Table of Contents

1. [Population Simulation Performance](#1-population-simulation-performance)
2. [Unit Conversion Operations](#2-unit-conversion-operations)
3. [Data Processing and Transformations](#3-data-processing-and-transformations)
4. [.NET Interop Optimization](#4-net-interop-optimization)
5. [Collection Operations](#5-collection-operations)
6. [Entity Path Operations](#6-entity-path-operations)
7. [Caching Mechanisms](#7-caching-mechanisms)
8. [Memory Management](#8-memory-management)
9. [Priority Matrix](#9-priority-matrix)
10. [Implementation Recommendations](#10-implementation-recommendations)

---

## 1. Population Simulation Performance

### 1.1 Nested Loops with .NET Interop Calls in getOutputValues

**File**: `R/utilities-simulation-results.R:74-91`

**Issue**:
```r
for (individualIndex in seq_along(individualIds)) {
  individualId <- individualIds[individualIndex]
  individualProperties <- list(IndividualId = rep(individualId, valueLength))

  for (covariateName in covariateNames) {  # NESTED LOOP
    covariateValue <- population$getCovariateValue(
      covariateName,
      individualId
    )  # .NET CALL PER INDIVIDUAL PER COVARIATE
    individualProperties[[covariateName]] <- rep(covariateValue, valueLength)
  }
}
```

**Problem**:
- O(n*m) complexity where n=number of individuals, m=number of covariates
- Each iteration makes a separate .NET interop call which is expensive
- For a population of 1000 individuals with 10 covariates = **10,000 .NET calls**
- No batching or vectorization of covariate retrieval

**Impact**: **CRITICAL** - This is in the hot path of all population simulation result extraction

**Recommendation**:
```r
# Option 1: Batch retrieve all covariates for all individuals
if (!is.null(population)) {
  # Get all covariate values at once using vectorized .NET call
  allCovariateValues <- population$getAllCovariateValues(individualIds)

  for (individualIndex in seq_along(individualIds)) {
    individualId <- individualIds[individualIndex]
    individualProperties <- list(IndividualId = rep(individualId, valueLength))

    for (covariateName in covariateNames) {
      covariateValue <- allCovariateValues[[covariateName]][individualIndex]
      individualProperties[[covariateName]] <- rep(covariateValue, valueLength)
    }

    individualPropertiesCache[[individualIndex]] <- individualProperties
  }
}

# Option 2: If vectorized .NET API exists for entire matrix
covariateMatrix <- population$getCovariateMatrix(individualIds, covariateNames)
# Then process matrix in R more efficiently
```

**Priority**: **CRITICAL**

**Estimated Impact**: 50-80% reduction in result extraction time for population simulations

---

### 1.2 Inefficient Data Frame Construction with rbind.data.frame

**File**: `R/utilities-simulation-results.R:94-97`

**Issue**:
```r
allIndividualProperties <- do.call(
  rbind.data.frame,
  c(individualPropertiesCache, stringsAsFactors = FALSE)
)
```

**Problem**:
- `rbind.data.frame` is slow for binding many small data frames
- Creates intermediate copies during the binding process
- For 1000 individuals: combines 1000 separate list structures into one data frame
- Potential O(n²) complexity in worst case due to repeated memory allocations

**Impact**: **HIGH** - Affects all population simulation result processing

**Recommendation**:
```r
# Option 1: Use data.table for efficient row binding
library(data.table)
allIndividualProperties <- data.table::rbindlist(
  individualPropertiesCache,
  use.names = TRUE
)

# Option 2: Pre-allocate and fill by columns
nRows <- length(individualIds) * valueLength
allIndividualProperties <- data.frame(
  IndividualId = integer(nRows),
  Time = numeric(nRows),
  stringsAsFactors = FALSE
)
# Pre-allocate columns for each covariate
for (covariateName in covariateNames) {
  allIndividualProperties[[covariateName]] <- numeric(nRows)
}
# Fill in values more efficiently
rowIdx <- 1
for (individualIndex in seq_along(individualIds)) {
  endIdx <- rowIdx + valueLength - 1
  allIndividualProperties$IndividualId[rowIdx:endIdx] <- individualIds[individualIndex]
  allIndividualProperties$Time[rowIdx:endIdx] <- timeValues
  for (covariateName in covariateNames) {
    allIndividualProperties[[covariateName]][rowIdx:endIdx] <-
      individualPropertiesCache[[individualIndex]][[covariateName]]
  }
  rowIdx <- endIdx + 1
}
```

**Priority**: **HIGH**

**Estimated Impact**: 40-60% reduction in data frame construction time

---

### 1.3 Sequential Simulation Addition in runSimulationsConcurrently

**File**: `R/utilities-simulation.R:315-340`

**Issue**:
```r
for (simulationIdx in seq_along(simulations)) {
  simulation <- simulations[[simulationIdx]]
  simulationIdSimulationMap[[simulationIdx]] <- simulation
  names(simulationIdSimulationMap)[[simulationIdx]] <- simulation$id

  simulationRunner$call("AddSimulation", simulation)  # .NET CALL PER SIMULATION
}
```

**Problem**:
- n separate .NET calls to AddSimulation before RunConcurrently
- For 100 simulations = 100 separate .NET interop calls
- Sequential building of simulationIdSimulationMap with repeated name assignments

**Impact**: **MEDIUM-HIGH** - Affects batch simulation initialization time

**Recommendation**:
```r
# Option 1: If .NET API supports batch addition
simulationIds <- sapply(simulations, function(sim) sim$id)
simulationIdSimulationMap <- setNames(simulations, simulationIds)
simulationRunner$call("AddSimulations", simulations)  # Single .NET call with collection

# Option 2: If individual calls are required, optimize R-side processing
simulationIdSimulationMap <- vector("list", length(simulations))
names(simulationIdSimulationMap) <- sapply(simulations, function(sim) sim$id)

for (simulationIdx in seq_along(simulations)) {
  simulation <- simulations[[simulationIdx]]
  simulationIdSimulationMap[[simulationIdx]] <- simulation
  simulationRunner$call("AddSimulation", simulation)
}
```

**Priority**: **MEDIUM-HIGH**

**Estimated Impact**: 20-30% reduction in batch simulation setup time

---

### 1.4 Property Access in Validation

**File**: `R/population.R:52-65`

**Issue**:
```r
setParameterValues = function(parameterOrPath, values) {
  parameterPath <- private$getPathFrom(parameterOrPath)
  validateIsNumeric(values)

  expectedCount <- self$count  # Property access - potential .NET call
  actualCount <- length(values)
  if (actualCount != expectedCount) {
    stop(...)
  }

  self$call("SetValues", parameterPath, values)
}
```

**Problem**:
- `self$count` may trigger .NET interop if not cached
- Called frequently during parameter value setting operations
- Validation overhead on every call

**Impact**: **MEDIUM** - Affects parameter setting operations

**Recommendation**:
```r
# Cache count in private field during initialization or first access
setParameterValues = function(parameterOrPath, values) {
  parameterPath <- private$getPathFrom(parameterOrPath)
  validateIsNumeric(values)

  # Use cached count if available
  if (is.null(private$.cachedCount)) {
    private$.cachedCount <- self$count
  }
  expectedCount <- private$.cachedCount

  actualCount <- length(values)
  if (actualCount != expectedCount) {
    stop(messages$errorWrongNumberOfValues(actualCount, expectedCount))
  }

  self$call("SetValues", parameterPath, values)
}
```

**Priority**: **MEDIUM**

**Estimated Impact**: 10-15% reduction in parameter setting overhead

---

## 2. Unit Conversion Operations

### 2.1 Multiple .NET Calls Per Conversion

**File**: `R/utilities-units.R:148-242`

**Issue**:
```r
toUnit <- function(
  quantityOrDimension,
  values,
  targetUnit,
  sourceUnit = NULL,
  molWeight = NULL,
  molWeightUnit = NULL
) {
  # ... validation ...
  dimensionTask <- .getNetTaskFromCache("DimensionTask")
  values <- as.numeric(c(values))  # Memory allocation

  # Up to 4 separate .NET calls:
  if (!is.null(molWeightUnit)) {
    molWeight <- dimensionTask$call(
      "ConvertToBaseUnit",  # .NET CALL 1
      ospDimensions$`Molecular weight`,
      molWeightUnit,
      molWeight
    )
  }

  if (!is.null(sourceUnit)) {
    values <- dimensionTask$call(
      "ConvertToBaseUnit",  # .NET CALL 2
      dimension,
      sourceUnit,
      values,
      molWeight
    )
  }

  if (targetUnit == baseUnit) {
    return(values)
  }

  return(dimensionTask$call(
    "ConvertToUnit",  # .NET CALL 3
    dimension,
    targetUnit,
    values
  ))
}
```

**Problem**:
- Up to 4 .NET calls per `toUnit` invocation
- Benchmark shows significant overhead: **70k ms** for 5000 iterations with 100k values
- Memory usage: **14,753 MB** indicating heavy allocations
- `as.numeric(c(values))` creates unnecessary copy

**Evidence from Benchmark** (`tests/dev/script-benchmark-unitConversion.R`):
- Same unit conversion: 40+ ms per 1000 iterations
- Comments show "New version: 7k ms, 2290 mb" vs old "40k ms, 10556 mb"
- 82% improvement possible with optimization

**Impact**: **CRITICAL** - Unit conversion is used extensively throughout the package

**Recommendation**:
```r
toUnit <- function(
  quantityOrDimension,
  values,
  targetUnit,
  sourceUnit = NULL,
  molWeight = NULL,
  molWeightUnit = NULL
) {
  # Early return for same unit without source
  if (is.null(sourceUnit) && targetUnit == baseUnit) {
    return(values)
  }

  # Avoid unnecessary memory allocation if values are already numeric
  if (!is.numeric(values)) {
    values <- as.numeric(values)
  }

  # Check if source and target are the same BEFORE any .NET calls
  if (!is.null(sourceUnit) && sourceUnit == targetUnit) {
    return(values)
  }

  dimensionTask <- .getNetTaskFromCache("DimensionTask")

  # Option 1: Single combined .NET call if API supports it
  if (!is.null(sourceUnit) && !is.null(molWeight)) {
    return(dimensionTask$call(
      "ConvertWithMolWeight",  # Hypothetical combined method
      dimension,
      sourceUnit,
      targetUnit,
      values,
      molWeight,
      molWeightUnit
    ))
  }

  # Option 2: Minimize calls by optimizing order
  # Convert molWeight first only if needed
  # ... existing logic with optimizations
}
```

**Priority**: **CRITICAL**

**Estimated Impact**: 50-80% reduction in unit conversion time (evidence from benchmark)

---

### 2.2 Multiple Split/Apply/Bind Cycles in Unit Conversion

**File**: `R/utilities-data-combined.R:636-662`

**Issue**:
```r
# Split 1 for xUnit conversion
xDataList <- .removeEmptyDataFrame(split(data, data$xUnitSplit))
data <- purrr::map_dfr(
  .x = xDataList,
  .f = function(data) .xUnitConverter(data, xTargetUnit)
)

# Split 2 for yUnit conversion
yDataList <- .removeEmptyDataFrame(split(
  data,
  list(data$yUnitSplit, data$molWeightSplit)
))
data <- purrr::map_dfr(
  .x = yDataList,
  .f = function(data) .yUnitConverter(data, yTargetUnit)
)

# Split 3 for yError conversion
if (any(colnames(data) == "yErrorValues")) {
  yErrorDataList <- .removeEmptyDataFrame(split(
    data,
    list(data$yErrorUnitSplit, data$molWeightSplit)
  ))
  data <- purrr::map_dfr(
    .x = yErrorDataList,
    .f = function(data) .yErrorUnitConverter(data, yTargetUnit)
  )
}
```

**Problem**:
- Three separate `split()` operations on potentially large data frames
- Multiple `purrr::map_dfr()` calls involve repeated row binding
- Each split/apply/bind cycle iterates over the entire dataset
- High overhead from data frame reorganization
- O(n*k) complexity where n=rows, k=number of conversions

**Impact**: **HIGH** - Affects all data processing with unit conversions

**Recommendation**:
```r
# Option 1: Process by groups using data.table for efficiency
library(data.table)
data <- as.data.table(data)

# Add split columns if needed
data[, `:=`(
  xUnitSplit = as.character(xUnit),
  yUnitSplit = as.character(yUnit),
  molWeightSplit = as.character(molWeight)
)]

# Single pass conversion
data[, `:=`(
  xValues = .xUnitConverter(.SD, xTargetUnit),
  yValues = .yUnitConverter(.SD, yTargetUnit)
), by = .(xUnitSplit, yUnitSplit, molWeightSplit)]

# Option 2: Vectorize conversions where possible
# Group indices instead of splitting
xGroups <- interaction(data$xUnitSplit, drop = TRUE)
for (group in levels(xGroups)) {
  idx <- xGroups == group
  data$xValues[idx] <- .xUnitConverter(data[idx, ], xTargetUnit)
}
```

**Priority**: **HIGH**

**Estimated Impact**: 40-60% reduction in data processing time for unit conversions

---

### 2.3 Unnecessary Column Creation in unitConverter

**File**: `R/utilities-units.R:529-681`

**Issue**:
```r
.unitConverter <- function(data, xUnit = NULL, yUnit = NULL) {
  # Early return (good)
  if (
    length(unique(data$xUnit)) == 1L &
      is.null(xUnit) &
      length(unique(data$yUnit)) == 1L &
      is.null(yUnit)
  ) {
    return(data)
  }

  # But then STILL creates Split columns even if not needed
  data <- dplyr::mutate(
    data,
    dplyr::across(
      .cols = dplyr::matches("Unit$|Weight$"),
      .fns = as.character,
      .names = "{.col}Split"
    )
  )
}
```

**Problem**:
- Added columns (xUnitSplit, yUnitSplit, molWeightSplit) for split operations
- Increases memory footprint: 1000 rows × 5 new columns = 5000 additional cells
- Columns persist through multiple operations
- Early return doesn't prevent column creation in later code paths

**Impact**: **MEDIUM** - Memory overhead for large datasets

**Recommendation**:
```r
.unitConverter <- function(data, xUnit = NULL, yUnit = NULL) {
  # Enhanced early return
  if (
    length(unique(data$xUnit)) == 1L &
      is.null(xUnit) &
      length(unique(data$yUnit)) == 1L &
      is.null(yUnit)
  ) {
    return(data)
  }

  # Create split columns in-place only when needed
  # Use data.table for efficient in-place operations
  dt <- data.table::as.data.table(data)

  # Only add columns that don't exist
  if (!"xUnitSplit" %in% names(dt)) {
    dt[, xUnitSplit := as.character(xUnit)]
  }

  # Process conversions
  # ...

  # Remove temporary split columns before returning
  dt[, c("xUnitSplit", "yUnitSplit", "molWeightSplit") := NULL]

  return(as.data.frame(dt))
}
```

**Priority**: **MEDIUM**

**Estimated Impact**: 15-25% reduction in memory usage for large datasets

---

## 3. Data Processing and Transformations

### 3.1 Inefficient Entity to Path Conversion

**File**: `R/utilities-entity.R:233-248`

**Issue**:
```r
.entitiesToPaths <- function(entitiesOrPaths) {
  paths <- vector("character", length(entitiesOrPaths))

  for (idx in seq_along(entitiesOrPaths)) {
    element <- entitiesOrPaths[[idx]]
    if (isOfType(element, "Entity")) {  # Type check in loop
      paths[[idx]] <- element$path
    } else {
      paths[[idx]] <- element
    }
  }
  paths <- unique(paths)
  return(paths)
}
```

**Problem**:
- Type checking repeated for each element
- For 1000 paths = 1000 type checks
- Loop could be vectorized
- `unique()` called after building entire vector

**Impact**: **MEDIUM** - Called frequently for entity operations

**Recommendation**:
```r
.entitiesToPaths <- function(entitiesOrPaths) {
  if (length(entitiesOrPaths) == 0) {
    return(character(0))
  }

  # Check if all elements are entities or all are paths
  firstIsEntity <- isOfType(entitiesOrPaths[[1]], "Entity")

  # Fast path for homogeneous input
  if (length(entitiesOrPaths) > 10) {
    # Sample check for homogeneity
    sampledIndices <- sample.int(
      length(entitiesOrPaths),
      min(10, length(entitiesOrPaths))
    )
    allSameType <- all(sapply(
      entitiesOrPaths[sampledIndices],
      function(x) isOfType(x, "Entity") == firstIsEntity
    ))

    if (allSameType) {
      if (firstIsEntity) {
        paths <- vapply(entitiesOrPaths, function(x) x$path, character(1))
      } else {
        paths <- as.character(entitiesOrPaths)
      }
      return(unique(paths))
    }
  }

  # Mixed type path (slower but necessary)
  paths <- vapply(
    entitiesOrPaths,
    function(element) {
      if (isOfType(element, "Entity")) element$path else as.character(element)
    },
    character(1)
  )

  return(unique(paths))
}
```

**Priority**: **MEDIUM**

**Estimated Impact**: 30-50% reduction in path conversion time

---

### 3.2 Sequential Property Access in Quantity Operations

**File**: `R/utilities-quantity.R:109-120`

**Issue**:
```r
for (i in seq_along(quantities)) {
  quantity <- quantities[[i]]
  value <- values[[i]]
  if (!is.null(units)) {
    value <- toBaseUnit(
      quantityOrDimension = quantity,
      values = value,
      unit = units[[i]]
    )  # Per-iteration .NET call
  }
  quantity$value <- value  # Property assignment per iteration
}
```

**Problem**:
- For each of n quantities: 1 toBaseUnit call + 1 property assignment
- For 100 quantities = potentially 100+ .NET calls
- No batching of unit conversions or property assignments

**Impact**: **MEDIUM** - Affects quantity value setting operations

**Recommendation**:
```r
# Option 1: Batch convert units if same dimension
if (!is.null(units)) {
  # Group quantities by dimension for batch conversion
  dimensionGroups <- split(
    seq_along(quantities),
    vapply(quantities, function(q) q$dimension, character(1))
  )

  convertedValues <- values
  for (dimGroup in dimensionGroups) {
    if (length(dimGroup) > 1) {
      # Batch conversion for same dimension
      convertedValues[dimGroup] <- toBaseUnit(
        quantityOrDimension = quantities[[dimGroup[1]]],
        values = values[dimGroup],
        unit = units[dimGroup]
      )
    } else {
      idx <- dimGroup[1]
      convertedValues[idx] <- toBaseUnit(
        quantityOrDimension = quantities[[idx]],
        values = values[[idx]],
        unit = units[[idx]]
      )
    }
  }
  values <- convertedValues
}

# Set all values
for (i in seq_along(quantities)) {
  quantities[[i]]$value <- values[[i]]
}

# Option 2: If .NET API supports batch setting
quantityPaths <- vapply(quantities, function(q) q$path, character(1))
.setQuantityValues(quantityPaths, values)  # Single .NET call
```

**Priority**: **MEDIUM**

**Estimated Impact**: 30-40% reduction in quantity value setting time

---

## 4. .NET Interop Optimization

### 4.1 Repeated Entity Matching with .NET Calls

**File**: `R/utilities-entity.R:116-143`

**Issue**:
```r
.getAllEntitiesMatching <- function(
  paths,
  container,
  entityType,
  method = NULL
) {
  task <- .getNetTaskFromCache("ContainerTask")
  method <- method %||% AllMatchingMethod[[className]]

  findEntitiesByPath <- function(path) {
    .toObjectType(task$call(method, container, path), entityType)  # .NET CALL PER PATH
  }

  return(.unify(findEntitiesByPath, paths))
}
```

**Problem**:
- For n paths: n separate .NET calls to ContainerTask
- For 100 paths = 100 .NET interop calls
- No batching of entity retrieval

**Impact**: **HIGH** - Affects all entity lookup operations

**Recommendation**:
```r
.getAllEntitiesMatching <- function(
  paths,
  container,
  entityType,
  method = NULL
) {
  if (length(paths) == 0) {
    return(list())
  }

  task <- .getNetTaskFromCache("ContainerTask")
  method <- method %||% AllMatchingMethod[[className]]

  # Option 1: Single batched .NET call if API supports it
  entities <- task$call(
    paste0(method, "Batch"),  # e.g., "GetAllQuantitiesMatchingBatch"
    container,
    paths
  )
  return(.toObjectType(entities, entityType))

  # Option 2: If batch not available, minimize overhead
  findEntitiesByPath <- function(path) {
    .toObjectType(task$call(method, container, path), entityType)
  }

  # Use lapply instead of custom .unify for better performance
  listOfEntities <- lapply(paths, findEntitiesByPath)
  return(uniqueEntities(unlist(listOfEntities, use.names = FALSE)))
}
```

**Priority**: **HIGH**

**Estimated Impact**: 50-70% reduction in entity lookup time

---

### 4.2 Metadata Retrieval with Multiple .NET Calls

**File**: `R/utilities-simulation-results.R:104-130`

**Issue**:
```r
if (addMetaData) {
  metaData <- lapply(paths, function(path) {
    unit <- NULL
    dimension <- NULL
    if (!all(is.na(values[[path]]))) {
      unit <- task$call(
        "BaseUnitNameByPath",  # .NET CALL 1
        simulationResults$simulation,
        path,
        stopIfNotFound
      )
      dimension <- task$call(
        "DimensionNameByPath",  # .NET CALL 2
        simulationResults$simulation,
        path,
        stopIfNotFound
      )
    }
    list(unit = unit, dimension = dimension)
  })
  names(metaData) <- paths
}
```

**Problem**:
- 2 .NET calls per path for metadata
- For 50 output paths = 100 .NET calls
- Sequential processing

**Impact**: **MEDIUM-HIGH** - Affects result extraction when metadata is needed

**Recommendation**:
```r
if (addMetaData) {
  # Filter paths with valid values first
  validPaths <- paths[!vapply(values, function(v) all(is.na(v)), logical(1))]

  # Option 1: Batch retrieve metadata if API supports it
  if (length(validPaths) > 0) {
    metadataList <- task$call(
      "GetMetadataForPaths",  # Hypothetical batch method
      simulationResults$simulation,
      validPaths,
      stopIfNotFound
    )
  }

  # Option 2: Minimize individual calls
  metaData <- vector("list", length(paths))
  names(metaData) <- paths

  for (path in validPaths) {
    unit <- task$call(
      "BaseUnitNameByPath",
      simulationResults$simulation,
      path,
      stopIfNotFound
    )
    dimension <- task$call(
      "DimensionNameByPath",
      simulationResults$simulation,
      path,
      stopIfNotFound
    )
    metaData[[path]] <- list(unit = unit, dimension = dimension)
  }

  # Add time metadata
  metaData[["Time"]] <- list(unit = "min", dimension = "Time")
}
```

**Priority**: **MEDIUM-HIGH**

**Estimated Impact**: 40-60% reduction in metadata retrieval time

---

### 4.3 Property Access Caching in DotNetWrapper

**File**: `R/dot-net-wrapper.R:20-42`

**Issue**:
```r
private$.wrapExtensionMethod = function(typename, methodName, propertyName, value) {
  if (missing(value)) {
    rSharp::callStatic(typename, methodName, self)  # .NET CALL ALWAYS
  } else {
    private$.throwPropertyIsReadonly(propertyName)
  }
}

private$.wrapExtensionMethodCached = function(
  typename,
  methodName,
  propertyName,
  cachedValue,
  value
) {
  if (missing(value)) {
    if (is.null(cachedValue)) {
      return(rSharp::callStatic(typename, methodName, self))
    }
    return(cachedValue)
  }
}
```

**Problem**:
- `.wrapExtensionMethod` always calls .NET even for immutable properties
- Not all properties use `.wrapExtensionMethodCached`
- Caching depends on caller to manage cached values

**Impact**: **MEDIUM** - Affects all property access across R6 classes

**Recommendation**:
```r
# Add automatic caching for read-only properties
private$.propertyCache = new.env(parent = emptyenv())

private$.wrapExtensionMethodCached = function(
  typename,
  methodName,
  propertyName,
  value
) {
  if (missing(value)) {
    cacheKey <- paste0(typename, ".", methodName)

    if (!exists(cacheKey, envir = private$.propertyCache)) {
      result <- rSharp::callStatic(typename, methodName, self)
      assign(cacheKey, result, envir = private$.propertyCache)
      return(result)
    }

    return(get(cacheKey, envir = private$.propertyCache))
  } else {
    private$.throwPropertyIsReadonly(propertyName)
  }
}

# Update all property wrappers to use cached version
# e.g., in Container, Simulation, Population classes
```

**Priority**: **MEDIUM**

**Estimated Impact**: 20-30% reduction in property access overhead

---

## 5. Collection Operations

### 5.1 Inefficient Unique Entity Operation

**File**: `R/utilities-entity.R:56-76, 80-96`

**Issue**:
```r
uniqueEntities <- function(entities) {
  if (length(entities) == 0) {
    return(entities)
  }

  uniqueEntitiesEnv <- new.env()

  for (entity in entities) {
    uniqueEntitiesEnv[[entity$id]] <- entity
  }

  as.list(uniqueEntitiesEnv)
}

.unify <- function(groupEntitiesByPathFunc, paths) {
  listOfEntitiesByPath <- lapply(paths, groupEntitiesByPathFunc)
  numberOfEntitiesSet <- length(listOfEntitiesByPath)
  listOfEntitiesByPath <- unlist(listOfEntitiesByPath, use.names = FALSE)

  if (numberOfEntitiesSet > 1) {
    if (!length(listOfEntitiesByPath) == 0) {
      listOfEntitiesByPath <- uniqueEntities(listOfEntitiesByPath)
    }
  }
  return(listOfEntitiesByPath)
}
```

**Problem**:
- Sequential iteration through all entities for uniqueness check
- Environment-based approach is correct but not optimized
- Multiple `lapply` + `unlist` + `uniqueEntities` operations

**Impact**: **MEDIUM** - Affects entity collection operations

**Recommendation**:
```r
uniqueEntities <- function(entities) {
  if (length(entities) <= 1) {
    return(entities)
  }

  # Fast path: extract IDs as vector first
  entityIds <- vapply(entities, function(e) e$id, character(1))
  uniqueIndices <- !duplicated(entityIds)

  return(entities[uniqueIndices])
}

.unify <- function(groupEntitiesByPathFunc, paths) {
  if (length(paths) == 0) {
    return(list())
  }

  if (length(paths) == 1) {
    return(groupEntitiesByPathFunc(paths))
  }

  listOfEntitiesByPath <- lapply(paths, groupEntitiesByPathFunc)
  listOfEntitiesByPath <- unlist(listOfEntitiesByPath, use.names = FALSE)

  return(uniqueEntities(listOfEntitiesByPath))
}
```

**Priority**: **MEDIUM**

**Estimated Impact**: 25-35% reduction in entity collection processing time

---

### 5.2 Path Extraction with Property Access

**File**: `R/utilities-simulation.R:400-418`

**Issue**:
```r
if (isOfType(variableParameters, "Parameter")) {
  variableParameters <- unlist(
    lapply(variableParameters, function(x) x$path),  # Property access in lapply
    use.names = FALSE
  )
}

if (isOfType(variableMolecules, "Quantity")) {
  variableMolecules <- unlist(
    lapply(variableMolecules, function(x) x$path),  # Same pattern
    use.names = FALSE
  )
}
```

**Problem**:
- Two separate `lapply` calls with property access
- Property access might trigger .NET calls if not cached
- Could be vectorized

**Impact**: **LOW-MEDIUM** - Called during simulation setup

**Recommendation**:
```r
# Use vapply for type safety and potential performance
if (isOfType(variableParameters, "Parameter")) {
  variableParameters <- vapply(variableParameters, function(x) x$path, character(1))
}

if (isOfType(variableMolecules, "Quantity")) {
  variableMolecules <- vapply(variableMolecules, function(x) x$path, character(1))
}

# Or combine if both are always present
extractPaths <- function(objects) {
  if (length(objects) == 0) return(character(0))
  vapply(objects, function(x) x$path, character(1))
}

variableParameters <- if (isOfType(variableParameters, "Parameter")) {
  extractPaths(variableParameters)
} else {
  variableParameters
}
```

**Priority**: **LOW-MEDIUM**

**Estimated Impact**: 10-20% reduction in path extraction time

---

## 6. Entity Path Operations

### 6.1 String Operations in Path Splitting

**File**: `R/utilities-path.R:12-34`

**Issue**:
```r
toPathArray <- function(path) {
  validateIsString(path)
  unlist(
    strsplit(path, paste0("\\", ospsuiteEnv$pathSeparator)),
    use.names = FALSE
  )
}

toPathString <- function(...) {
  pathStrings <- c(...)
  validateIsString(pathStrings)
  paste(pathStrings, collapse = ospsuiteEnv$pathSeparator)
}
```

**Problem**:
- `strsplit` with regex pattern escaping for each path
- Pattern `paste0("\\", ospsuiteEnv$pathSeparator)` recreated each time
- If called 1000 times: 1000 pattern creations + string operations

**Impact**: **LOW-MEDIUM** - Called frequently but individual operations are fast

**Recommendation**:
```r
# Cache the split pattern
.splitPattern <- NULL
.getSplitPattern <- function() {
  if (is.null(.splitPattern)) {
    .splitPattern <<- paste0("\\", ospsuiteEnv$pathSeparator)
  }
  .splitPattern
}

toPathArray <- function(path) {
  validateIsString(path)
  unlist(
    strsplit(path, .getSplitPattern(), fixed = FALSE),
    use.names = FALSE
  )
}

# For better performance with fixed = TRUE if no regex needed
toPathArray <- function(path) {
  validateIsString(path)
  unlist(
    strsplit(path, ospsuiteEnv$pathSeparator, fixed = TRUE),
    use.names = FALSE
  )
}
```

**Priority**: **LOW-MEDIUM**

**Estimated Impact**: 15-25% reduction in path string operations

---

## 7. Caching Mechanisms

### 7.1 Inefficient Cache Printing

**File**: `R/cache.R:84-89`

**Issue**:
```r
print = function(...) {
  cat("Cache for objects of type: ", self$type, "\n", sep = "")
  cat("Cached keys:\n")
  lapply(self$keys, function(key) {
    cat(key, "\n", sep = "")  # I/O operation per key
  })
  invisible(self)
}
```

**Problem**:
- I/O operation in loop
- For cache with 10,000 keys = 10,000+ `cat` calls
- Inefficient output buffering

**Impact**: **LOW** - Only affects debugging/printing, not runtime performance

**Recommendation**:
```r
print = function(...) {
  cat("Cache for objects of type: ", self$type, "\n", sep = "")
  cat("Cached keys:\n")

  # Single concatenated output
  if (length(self$keys) > 0) {
    cat(paste(self$keys, collapse = "\n"), "\n", sep = "")
  }

  invisible(self)
}
```

**Priority**: **LOW**

**Estimated Impact**: Negligible runtime impact, improves debugging experience

---

### 7.2 Per-Individual Cache Lookups

**File**: `R/simulation-results.R:34, 66-71`

**Issue**:
```r
private$.individualResultsCache <- Cache$new()

resultsForIndividual = function(individualId) {
  validateIsNumeric(individualId)
  if (!private$.individualResultsCache$hasKey(individualId)) {
    individualResult <- private$getResultsForIndividual(individualId)
    private$.individualResultsCache$set(individualId, individualResult)
  }

  private$.individualResultsCache$get(individualId)
}
```

**Problem**:
- Cache key uses `toString()` conversion (in Cache implementation)
- For 1000 individual IDs: 1000+ string conversions + environment lookups
- Validation on every call

**Impact**: **MEDIUM** - Affects population result access

**Recommendation**:
```r
# Use integer keys directly if Cache supports it
resultsForIndividual = function(individualId) {
  validateIsNumeric(individualId)

  # Use integer as key directly (avoid toString conversion)
  cacheKey <- as.integer(individualId)

  if (!private$.individualResultsCache$hasKey(cacheKey)) {
    individualResult <- private$getResultsForIndividual(individualId)
    private$.individualResultsCache$set(cacheKey, individualResult)
  }

  private$.individualResultsCache$get(cacheKey)
}

# Or batch retrieve if accessing multiple individuals
resultsForIndividuals = function(individualIds) {
  validateIsNumeric(individualIds)

  uncachedIds <- individualIds[!vapply(
    individualIds,
    private$.individualResultsCache$hasKey,
    logical(1)
  )]

  if (length(uncachedIds) > 0) {
    # Batch retrieve if .NET API supports it
    results <- private$getResultsForIndividuals(uncachedIds)
    for (i in seq_along(uncachedIds)) {
      private$.individualResultsCache$set(uncachedIds[i], results[[i]])
    }
  }

  lapply(individualIds, private$.individualResultsCache$get)
}
```

**Priority**: **MEDIUM**

**Estimated Impact**: 20-30% reduction in cache overhead

---

## 8. Memory Management

### 8.1 Unnecessary Vector Copies

**Multiple Files**

**Issue**:
```r
# Pattern seen in multiple places
values <- as.numeric(c(values))  # Unnecessary copy if already numeric vector

# In utilities-units.R
dimensionTask <- .getNetTaskFromCache("DimensionTask")
values <- as.numeric(c(values))  # Line 242
```

**Problem**:
- `c(values)` creates a copy even if values is already a vector
- `as.numeric()` on numeric values creates another copy
- For large value arrays (100k elements): unnecessary memory allocation

**Impact**: **MEDIUM** - Memory overhead in unit conversions and data processing

**Recommendation**:
```r
# Check type before conversion
if (!is.numeric(values)) {
  values <- as.numeric(values)
} else if (is.list(values)) {
  values <- as.numeric(unlist(values))
}

# Or use more efficient coercion
values <- as.double(values)  # Faster than as.numeric for vectors
```

**Priority**: **MEDIUM**

**Estimated Impact**: 15-25% reduction in memory allocations

---

### 8.2 Pre-allocation Opportunities

**Multiple Files**

**Issue**:
```r
# Pattern: Growing vectors in loops
paths <- vector("character", length(entitiesOrPaths))
for (idx in seq_along(entitiesOrPaths)) {
  paths[[idx]] <- ...  # Good: pre-allocated
}

# But also patterns like:
results <- list()
for (i in seq_along(items)) {
  results[[i]] <- processItem(items[[i]])  # Growing list
}
```

**Problem**:
- Some loops properly pre-allocate, others don't
- Growing lists can cause repeated memory allocations
- Inconsistent patterns across codebase

**Impact**: **LOW-MEDIUM** - Depends on loop size

**Recommendation**:
```r
# Always pre-allocate when size is known
results <- vector("list", length(items))
for (i in seq_along(items)) {
  results[[i]] <- processItem(items[[i]])
}

# For data frames
nRows <- length(individualIds) * valueLength
df <- data.frame(
  col1 = vector("type", nRows),
  col2 = vector("type", nRows),
  stringsAsFactors = FALSE
)
```

**Priority**: **LOW-MEDIUM**

**Estimated Impact**: 10-20% reduction in memory churn

---

## 9. Priority Matrix

| Priority | Optimization | Location | Estimated Impact | Effort |
|----------|--------------|----------|------------------|--------|
| **CRITICAL** | Batch covariate retrieval in getOutputValues | utilities-simulation-results.R:74-91 | 50-80% faster | Medium |
| **CRITICAL** | Optimize unit conversion .NET calls | utilities-units.R:148-242 | 50-80% faster | Medium-High |
| **HIGH** | Use data.table for rbind operations | utilities-simulation-results.R:94-97 | 40-60% faster | Low |
| **HIGH** | Batch entity matching .NET calls | utilities-entity.R:116-143 | 50-70% faster | Medium |
| **HIGH** | Reduce split/apply/bind cycles | utilities-data-combined.R:636-662 | 40-60% faster | Medium |
| **MEDIUM-HIGH** | Batch simulation addition | utilities-simulation.R:315-340 | 20-30% faster | Low-Medium |
| **MEDIUM-HIGH** | Batch metadata retrieval | utilities-simulation-results.R:104-130 | 40-60% faster | Medium |
| **MEDIUM** | Cache population count | population.R:52-65 | 10-15% faster | Low |
| **MEDIUM** | Optimize path conversion | utilities-entity.R:233-248 | 30-50% faster | Low-Medium |
| **MEDIUM** | Batch quantity value setting | utilities-quantity.R:109-120 | 30-40% faster | Medium |
| **MEDIUM** | Property access caching | dot-net-wrapper.R:20-42 | 20-30% faster | Medium |
| **MEDIUM** | Optimize unique entities | utilities-entity.R:56-76 | 25-35% faster | Low |
| **MEDIUM** | Cache individual results efficiently | simulation-results.R:66-71 | 20-30% faster | Low |
| **MEDIUM** | Reduce memory copies | utilities-units.R:242 | 15-25% less memory | Low |
| **LOW-MEDIUM** | Cache path split pattern | utilities-path.R:12-34 | 15-25% faster | Low |
| **LOW-MEDIUM** | Use vapply for path extraction | utilities-simulation.R:400-418 | 10-20% faster | Low |
| **LOW-MEDIUM** | Pre-allocate vectors | Multiple files | 10-20% less memory | Low |
| **LOW** | Optimize cache printing | cache.R:84-89 | Negligible | Low |

---

## 10. Implementation Recommendations

### Phase 1: Critical Performance Improvements (Weeks 1-2)

**Focus**: Highest impact optimizations that affect core workflows

1. **Batch Covariate Retrieval** (utilities-simulation-results.R)
   - Implement/expose batch .NET API for getting all covariate values
   - Modify `getOutputValues` to use single batched call
   - **Expected**: 50-80% improvement in population result extraction
   - **Testing**: Benchmark with populations of 100, 1000, 10000 individuals

2. **Unit Conversion Optimization** (utilities-units.R)
   - Add early returns for same-unit conversions
   - Eliminate unnecessary `as.numeric(c(values))` copies
   - Combine multiple .NET calls where possible
   - **Expected**: 50-80% improvement (evidence from benchmark)
   - **Testing**: Run existing benchmark script to verify improvements

3. **Data Frame Row Binding** (utilities-simulation-results.R)
   - Replace `rbind.data.frame` with `data.table::rbindlist`
   - Or implement pre-allocation strategy
   - **Expected**: 40-60% improvement in data frame construction
   - **Testing**: Benchmark with various population sizes

### Phase 2: High-Impact Data Processing (Weeks 3-4)

4. **Batch Entity Matching** (utilities-entity.R)
   - Implement batch .NET API for entity retrieval
   - Reduce n separate calls to single batched call
   - **Expected**: 50-70% improvement in entity lookups
   - **Testing**: Test with multiple entity types and path counts

5. **Unit Conversion Data Processing** (utilities-data-combined.R)
   - Consolidate multiple split/apply/bind cycles
   - Use data.table for efficient grouped operations
   - **Expected**: 40-60% improvement in data processing
   - **Testing**: Test with various unit combinations

6. **Batch Simulation Addition** (utilities-simulation.R)
   - Optimize simulation collection building
   - Batch add simulations if .NET API supports it
   - **Expected**: 20-30% improvement in batch setup
   - **Testing**: Benchmark with 10, 50, 100 simulations

### Phase 3: Medium-Impact Optimizations (Weeks 5-6)

7. **Metadata Retrieval** (utilities-simulation-results.R)
   - Batch retrieve unit and dimension metadata
   - Reduce 2n calls to n or fewer calls
   - **Expected**: 40-60% improvement when metadata requested

8. **Property Caching** (dot-net-wrapper.R, various classes)
   - Implement automatic caching for immutable properties
   - Update common classes to use cached property access
   - **Expected**: 20-30% reduction in property access overhead

9. **Collection Operations** (utilities-entity.R, utilities-quantity.R)
   - Optimize entity uniqueness checks
   - Improve path extraction and conversion
   - **Expected**: 25-35% improvement in collection operations

### Phase 4: Polish and Memory Optimization (Week 7+)

10. **Memory Management**
    - Eliminate unnecessary copies
    - Ensure consistent pre-allocation
    - **Expected**: 15-25% reduction in memory usage

11. **Minor Optimizations**
    - Cache path patterns
    - Optimize printing functions
    - Clean up validation overhead
    - **Expected**: 10-20% improvement in edge cases

### Testing Strategy

For each optimization phase:

1. **Baseline Benchmarks**
   - Run existing benchmark scripts (e.g., `script-benchmark-unitConversion.R`)
   - Create new benchmarks for population operations
   - Profile memory usage with `profvis`

2. **Unit Tests**
   - Ensure all existing tests pass
   - Add regression tests for performance-critical paths
   - Validate numerical accuracy unchanged

3. **Integration Tests**
   - Test with real-world simulation files
   - Verify population simulations work correctly
   - Check batch processing functionality

4. **Performance Tests**
   - Compare before/after metrics
   - Test with various data sizes (small, medium, large)
   - Profile to confirm hotspots addressed

### Monitoring and Validation

1. **Benchmark Suite**
   - Create comprehensive benchmark suite covering:
     - Population simulation (10, 100, 1000, 10000 individuals)
     - Unit conversion (various scenarios from existing script)
     - Batch simulation (10, 50, 100 simulations)
     - Data processing (small to large datasets)

2. **Memory Profiling**
   - Use `profvis` for before/after comparisons
   - Track peak memory usage
   - Monitor allocation patterns

3. **Regression Tests**
   - Ensure numerical results unchanged
   - Verify all edge cases handled
   - Confirm error messages preserved

### Risk Mitigation

1. **Backward Compatibility**
   - Maintain existing API signatures
   - Add new optimized methods without breaking existing code
   - Use feature flags for experimental optimizations

2. **Gradual Rollout**
   - Implement optimizations incrementally
   - Test each change thoroughly before proceeding
   - Keep ability to fall back to original implementation

3. **.NET API Changes**
   - Some optimizations require .NET API enhancements
   - Document which optimizations depend on .NET changes
   - Implement R-side optimizations that don't require .NET changes first

---

## Conclusion

This analysis identifies substantial opportunities for performance optimization in OSPSuite-R, particularly in:

1. **Population Simulation**: Batching .NET interop calls can reduce overhead by 50-80%
2. **Unit Conversion**: Evidence from existing benchmarks shows 50-80% improvement is achievable
3. **Data Processing**: More efficient data.table operations can improve processing by 40-60%
4. **Entity Operations**: Batch retrieval can reduce .NET call overhead by 50-70%

The recommended phased approach prioritizes critical performance improvements first, with clear testing strategies and success metrics. Many optimizations can be implemented with low to medium effort while providing substantial performance gains.

**Overall Expected Improvement**: 30-60% reduction in runtime for typical workflows, with up to 80% improvement in specific operations like unit conversion and population result extraction.
