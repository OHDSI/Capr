
# Concept set related classes ------------------------------------------

#' An S4 class for a single OMOP Concept
#'
#' A concept class contains all the information about the concept from the OMOP voabulary.
#'
#' @slot concept_id the OMOP/OHDSI concept ID
#' @slot concept_name the name of the concept
#' @slot standard_concept whether the concept is standard 'S', classification 'C', or  non-standard NA
#' @slot standard_concept_caption Whether the concept is standard full phrase
#' @slot invalid_reason Whether the concept is invalid single letter
#' @slot invalid_reason_caption whether the concept is invalid standard phrase
#' @slot concept_code The original code of the concept from its vocabulary
#' @slot domain_id The domain of the concept (e.g. Drug, Condition, Procedure, etc)
#' @slot vocabulary_id the name of the vocabulary
#' @slot concept_class_id type of concept class
setClass("Concept",
         slots = c(concept_id = "integer",
                   concept_name = "character",
                   standard_concept = "character",
                   standard_concept_caption = "character",
                   invalid_reason = "character",
                   invalid_reason_caption = "character",
                   concept_code = "character",
                   domain_id = "character",
                   vocabulary_id = "character",
                   concept_class_id = "character"),
         prototype = list(concept_id = 0L,
                          concept_name = NA_character_,
                          standard_concept = NA_character_,
                          standard_concept_caption = NA_character_,
                          invalid_reason = NA_character_,
                          invalid_reason_caption = NA_character_,
                          concept_code = NA_character_,
                          domain_id = NA_character_,
                          vocabulary_id = NA_character_,
                          concept_class_id = NA_character_)
)

setValidity("Concept", function(object) {
  stopifnot(is.integer(object@concept_id),
            object@concept_id >= 0,
            is.character(object@concept_name),
            length(object@concept_name) == 1)
  TRUE
})


setMethod("show", "Concept", function(object) {
  nm <- methods::slotNames(methods::is(object))
  concept <- unname(sapply(nm, methods::slot, object = object))
  cid <- paste("conceptId:", concept[1])
  cname <- paste("conceptName:", concept[2])
  cstd <- paste("standardConcept:", concept[3])
  cdom <- paste("domainId:", concept[8])
  cat("",cid, "\n", cname, "\n", cstd,"\n", cdom,"\n")
})

#' An S4 class for ConceptSetItem
#'
#' A class that provides information on the mapping of the concept
#'
#' @slot Concept a concept class object
#' @slot isExcluded toggle if want to exclude the concept
#' @slot includeDescendants toggle if want to include descendants
#' @slot includeMapped toggle if want to include map
setClass("ConceptSetItem",
         slots = c(Concept = "Concept",
                   isExcluded = "logical",
                   includeDescendants = "logical",
                   includeMapped = "logical"),
         prototype = list(Concept = new("Concept"),
                          isExcluded = FALSE,
                          includeDescendants = FALSE,
                          includeMapped = FALSE)
)

setValidity("ConceptSetItem", function(object) {
  stopifnot(is(object@Concept, "Concept"))
  stopifnot(is.logical(object@isExcluded),
            length(object@isExcluded) == 1,
            is.logical(object@includeDescendants),
            length(object@includeDescendants) == 1,
            is.logical(object@includeMapped),
            length(object@includeMapped) == 1)

  TRUE
})


# print a checkmark
# stringi::stri_unescape_unicode("\\u2714")
setMethod("show", "ConceptSetItem", function(object) {
  id <- object@Concept@concept_id
  exc <- ifelse(object@isExcluded, "-", "")
  des <- ifelse(object@includeDescendants, "*", "")
  mapped <- ifelse(object@includeMapped, "m", "")
  paste0("<", exc, id, mapped, des, ">")
})



#' An S4 class for ConceptSetExpresion
#'
#' A class for the concept set expressions bundles multiple concepts with mapping
#'
#' @slot id an id for the concept set expression to identify within a component
#' @slot Name the name of the concept set expression
#' @slot Expression a list containing expressions. expressions include multiple conceptSetItem objects
setClass("ConceptSet",
         slots = c(id = "ANY",
                   Name = "character",
                   Expression = "list"))

setValidity("ConceptSet", function(object) {
  stopifnot(is.character(object@id),
            length(object@id) == 1,
            #is.character(object@id),
            length(object@id) == 1,
            is.list(object@Expression),
            all(purrr::map_lgl(object@Expression, ~is(., "ConceptSetItem")))
            )
  TRUE
})

setMethod("show", "ConceptSet", function(object) {
  cli::cat_rule(paste("<Capr Concept Set>", object@Name))
  print(as.data.frame(object))
})

# Constructors ------------------------
# User facing concept set constructors
# From the user's perspective there are only concept sets with zero or more members.

# Internal
newConcept <- function(id,
                       isExcluded = FALSE,
                       includeDescendants = FALSE,
                       includeMapped = FALSE,
                       conceptName = "",
                       standardConcept = "",
                       standardConceptCaption = "",
                       invalidReason = "",
                       invalidReasonCaption = "",
                       conceptCode = "",
                       domainId = "",
                       vocabularyId = "",
                       conceptClassId = "") {
  checkmate::assertIntegerish(id, len = 1)

  concept <- methods::new("Concept",
                 concept_id = as.integer(id),
                 concept_name = conceptName,
                 standard_concept = standardConcept,
                 standard_concept_caption = standardConceptCaption,
                 invalid_reason = invalidReason,
                 invalid_reason_caption = invalidReasonCaption,
                 concept_code = conceptCode,
                 domain_id = domainId,
                 vocabulary_id = vocabularyId,
                 concept_class_id = conceptClassId)

  res <- methods::new("ConceptSetItem",
      Concept = concept,
      isExcluded = isExcluded,
      includeDescendants = includeDescendants,
      includeMapped = includeMapped)
  return(res)
}


#' Create a concept set
#'
#' cs is used to create concept set expressions.
#'
#' @param ... One or more numeric vectors that can be coerced to integers, or
#' Calls to helper functions "exclude", "descendants", or "mapped". Negative
#' integers will be marked as excluded from the concept set.
#' @param name A name for the concept set
#' @param id An id for the concept set
#'
#' @return A Capr Concept Set Object
#' @export
#'
#' @examples
#' \dontrun{
#' cs(1, 2)
#' cs(1, c(1, 10, 2))
#' cs(1, seq(2, 10, 2))
#' cs(1, 2, 3, exclude(4, 5))
#' cs(1, 2, 3, exclude(4, 5), mapped(6, 7))
#' cs(1, 2, 3, exclude(4, 5), mapped(6, 7), descendants(8, 9))
#' cs(descendants(1, 2, 3),  exclude(descendants(8, 9)))
#' }
cs <- function(..., name = "", id = NULL) {
  dots <- unlist(list(...), recursive = F)

  conceptList <- lapply(dots, function(x) {
    if (is.numeric(x) && length(x) == 1) {
      if (x < 0) {
        return(newConcept(x, isExcluded = TRUE))
      } else {
        return(newConcept(x))
      }
    } else if (methods::is(x, "ConceptSetItem") || is.null(x)) {
      return(x)
    }
  })

  ids <- purrr::map_int(conceptList, ~.@Concept@concept_id)
  dups <- ids[duplicated(ids)]
  if (length(dups) > 0) {
    rlang::abort(paste("ID: ", paste(dups, collapse = ", "), " are duplicated in the concept set."))
  }

  # TODO decide how to handle duplicate ids in `cs`. For now we throw error.

  if (is.null(id)) {
    id <- purrr::map_chr(conceptList, ~paste0(.@Concept@concept_id,
                                              .@isExcluded,
                                              .@includeDescendants,
                                              .@includeMapped)) %>%
      sort() %>%
      paste0(collapse = "") %>%
      digest::digest(algo = "md5") %>%
      as.character()
  }

  methods::new("ConceptSet",
               id = id,
               Name = name,
               Expression = conceptList)
}

# Constructor helpers -----

#' Exclude concepts from a concept set
#'
#' `exclude` is meant to be used inside `cs` when creating a new concept set.
#'
#' @param ... One or more numeric vectors that can be coerced to integers, or
#' Calls to helper functions "exclude", "descendants", or "mapped".
#'
#' @return A list of Capr concepts
#' @export
#' @describeIn cs exclude concepts
exclude <- function(...) {
  dots <- unlist(list(...), recursive = F)

  lapply(dots, function(x) {
    if (is.numeric(x) && length(x) == 1) {
      return(newConcept(x, isExcluded = TRUE))
    } else if (methods::is(x, "ConceptSetItem")) {
      x@isExcluded <- TRUE
      return(x)
    }
  })
}

#' Include mapped concepts in a concept set
#'
#' `mapped` is meant to be used inside `cs` when creating a new concept set.
#'
#' @param ... One or more numeric vectors that can be coerced to integers, or
#' Calls to helper functions "exclude", "descendants", or "mapped".
#'
#' @return A list of Capr concepts
#' @export
#' @describeIn cs Include mapped concepts
mapped <- function(...) {
  dots <- unlist(list(...), recursive = F)

  lapply(dots, function(x) {
    if (is.numeric(x) && length(x) == 1) {
      return(newConcept(x, includeMapped = TRUE))
    } else if (methods::is(x, "ConceptSetItem")) {
      x@includeMapped <- TRUE
      return(x)
    }
  })
}

#' Include descendants in a concept set
#'
#' `descendants` is meant to be used inside `cs` when creating a new concept set.
#'
#' @param ... One or more numeric vectors that can be coerced to integers, or
#' Calls to helper functions "exclude", "descendants", or "mapped".
#'
#' @return A list of Capr concepts
#' @export
#' @describeIn cs Include descendants
descendants <- function(...) {
  dots <- unlist(list(...), recursive = F)

  lapply(dots, function(x) {
    if (is.numeric(x) && length(x) == 1) {
      return(newConcept(x, includeDescendants = TRUE))
    } else if (methods::is(x, "ConceptSetItem")) {
      x@includeDescendants <- TRUE
      return(x)
    }
  })
}

# Type Coercion ----

#' Coerce a concept set expression to a dataframe
#'
#' @param x A Caper Concept Set
#'
#' @return A tibble (dataframe) with columns: concept_id, includeDescendants, isExcluded, includeMapped.
setMethod("as.data.frame", "ConceptSet", function(x) {
  df <- tibble::tibble(
    conceptId = purrr::map_int(x@Expression, ~.@Concept@concept_id),
    conceptCode = purrr::map_chr(x@Expression, ~.@Concept@concept_code),
    conceptName = purrr::map_chr(x@Expression, ~.@Concept@concept_name),
    domainId = purrr::map_chr(x@Expression, ~.@Concept@domain_id),
    vocabularyId = purrr::map_chr(x@Expression, ~.@Concept@vocabulary_id),
    standardConcept = purrr::map_chr(x@Expression, ~.@Concept@standard_concept),
    includeDescendants = purrr::map_lgl(x@Expression, "includeDescendants"),
    isExcluded = purrr::map_lgl(x@Expression, "isExcluded"),
    includeMapped = purrr::map_lgl(x@Expression, "includeMapped")
  )


})

setMethod("as.list", "Concept", function(x){
  nm <- methods::slotNames(methods::is(x))
  concept <- lapply(nm, methods::slot, object = x)
  # Convert NA_character to empty string
  concept <- lapply(concept, function(.) ifelse(is.character(.) && is.na(.), "", .))
  names(concept) <- toupper(nm)
  return(concept)
})

# as.list(x@Expression[[1]]@Concept)

setMethod("as.list", "ConceptSetItem", function(x){
  list('concept' = as.list(x@Concept),
       'isExcluded' = x@isExcluded,
       'includeDescendants' = x@includeDescendants,
       'includeMapped' = x@includeMapped)
})

# as.list(x@Expression[[1]])

setMethod("as.list", "ConceptSet", function(x){
            list('id' = x@id,
                 'name' = x@Name,
                 'expression' = list('items' = lapply(x@Expression, as.list)))
})

#' Coerce Capr object to json
#' @param x the capr object
#' @param pretty a toggle to make the json look nice, part of jsonlite
#' @param ... additional arguments passes to jsonlite::toJSON
#' @export
#' @docType methods
setGeneric("as.json", function(x, pretty = TRUE, ...)  standardGeneric("as.json"))

#' @rdname as.json
#' @aliases as.json,ConceptSet-method
setMethod("as.json", "ConceptSet", function(x, pretty = TRUE, ...){
  items <- list(items = lapply(x@Expression, as.list))
  jsonlite::toJSON(x = items, pretty = pretty, auto_unbox = TRUE, ...)
})

#' Save a concept set as a json file
#'
#' The resulting concept Set JSON file can be imported into Atlas.
#'
#' @param x A Capr concept set created by `cs()`
#' @param path Name of file to write to. (e.g. "concepts.json")
#' @param format the file extension to write
#' @param ... additional arguments
#'
#' @export
#'
#' @examples
#' \dontrun{
#' anemia <- cs(descendants(439777,4013073,4013074))
#' writeConceptSet(anemia, 'anemia.json')
#' writeConceptSet(anemia, 'anemia.csv')
#' }
writeConceptSet <- function(x, path, format = "auto", ...) {
  checkmate::assertChoice(format, choices = c("auto", "json", "csv"))
  if (format == "auto") {

    ext <- fs::path_ext(path)
    if (ext %in% c("json", "csv")) {
      format <- ext
    } else {
      rlang::abort("Unable to auto-detect file format. `path` must end with '.json' or '.csv'.")
    }
  }

  if (format == "json") {
    items <- list(items = lapply(x@Expression, as.list))
    jsonlite::write_json(x = items, path = path, pretty = TRUE, auto_unbox = TRUE, ...)
  } else if (format == "csv") {
    df <- tibble::tibble(
      concept_set_name = x@Name,
      concept_id = purrr::map_int(x@Expression, ~.@Concept@concept_id),
      concept_name = purrr::map_chr(x@Expression, ~.@Concept@concept_name),
      domain_id = purrr::map_chr(x@Expression, ~.@Concept@domain_id),
      vocabulary_id = purrr::map_chr(x@Expression, ~.@Concept@vocabulary_id),
      concept_class_id = purrr::map_chr(x@Expression, ~.@Concept@concept_class_id),
      standard_concept = purrr::map_chr(x@Expression, ~.@Concept@standard_concept),
      standard_concept_caption = purrr::map_chr(x@Expression, ~.@Concept@standard_concept_caption),
      concept_code = purrr::map_chr(x@Expression, ~.@Concept@concept_code),
      invalid_reason = purrr::map_chr(x@Expression, ~.@Concept@invalid_reason),
      invalid_reason_caption = purrr::map_chr(x@Expression, ~.@Concept@invalid_reason_caption),
      includeDescendants = purrr::map_lgl(x@Expression, "includeDescendants"),
      isExcluded = purrr::map_lgl(x@Expression, "isExcluded"),
      includeMapped = purrr::map_lgl(x@Expression, "includeMapped")
    )
    readr::write_csv(df, path, ...)
  }

  invisible(x)
}

#' Read a concept set json or csv into R
#'
#' Concept sets can be serialized to json or csv file formats. `readConceptSet`
#' reads the files into R as Capr concepts sets.
#'
#' @param path Name of concept set file to read in csv or json format. (e.g. "concepts.json")
#' @param name the name of the concept set
#' @param id the id for the concept set (keep?)
#' @export
#' @importFrom rlang %||%
#'
#' @examples
#' \dontrun{
#' anemia <- readConceptSet('anemia.json')
#' anemia <- readConceptSet('anemia.csv')
#' }
readConceptSet <- function(path, name, id = NULL) {

  checkmate::assertFileExists(path)

  ext <- tolower(fs::path_ext(path))
  if (!(ext %in% c("json", "csv"))) {
    rlang::abort("Unable to auto-detect file format. `path` must end with '.json' or '.csv'.")
  }

  if (missing(name)) {
    name <- fs::path_ext_remove(basename(path))
  } else {
    checkmate::assertCharacter(name, len = 1)
  }

  if (ext == "json") {
    items <- jsonlite::read_json(path = path)
    # TODO do some validation of the input. Also check that this works with Atlas.
    conceptList <- purrr::map(items[[1]], function(.) {
      newConcept(id = .$concept$CONCEPT_ID,
                 isExcluded = .$isExcluded,
                 includeDescendants = .$includeDescendants,
                 includeMapped = .$includeMapped,
                 conceptName = .$concept$CONCEPT_NAME,
                 standardConcept = .$concept$STANDARD_CONCEPT,
                 standardConceptCaption = .$concept$STANDARD_CONCEPT_CAPTION,
                 invalidReason = .$concept$INVALID_REASON,
                 conceptCode = .$concept$CONCEPT_CODE,
                 domainId = .$concept$DOMAIN_ID,
                 vocabularyId = .$concept$VOCABULARY_ID,
                 conceptClassId = .$concept$CONCEPT_CLASS_ID)
    })

  } else if (ext == "csv") {
    df <- readr::read_csv(path, show_col_types = FALSE)

    names(df) <- tolower(names(df))

    if (is.null(df[["concept_id"]] %||% df[["concept id"]])) {
      rlang::abort("`concept_id` cannot be missing in the input csv file")
    }

    # prefer a name in the csv file if it exists
    name <- df[["name"]][1] %||%
      df[["concept_set_name"]][1] %||%
      name

    if (is.na(name) || is.null(name)) {
      name <- ""
    }

    conceptDf <- tibble::tibble(
      id = df[["concept_id"]] %||% df[["concept id"]] %>% as.integer(),
      isExcluded = df[["isexcluded"]] %||% df[["exclude"]] %||% FALSE %>% as.logical(),
      includeDescendants = df[["includedescendants"]] %||% df[["descendants"]] %||% FALSE %>% as.logical(),
      includeMapped = df[["includemapped"]] %||% df[["mapped"]] %||% FALSE %>%  as.logical(),
      conceptName = df[["concept_name"]] %||% df[["concept name"]] %||% "" %>% as.character(),
      standardConcept = df[["standard_concept"]] %||% df[["standard concept"]] %||% "" %>% as.character(),
      standardConceptCaption = df[["standard_concept_caption"]] %||% "" %>% as.character(),
      invalidReason = df[["invalid_reason"]] %||% "" %>% as.character(),
      invalidReasonCaption = df[["invalid_reason_caption"]] %||% "" %>% as.character(),
      conceptCode = df[["concept_code"]] %||%  df[["concept code"]] %||% "" %>% as.character(),
      domainId = df[["domain_id"]] %||%  df[["domain"]] %||% "" %>% as.character(),
      vocabularyId = df[["vocabulary_id"]] %||%  df[["vocabulary"]] %||% "" %>% as.character(),
      conceptClassId = df[["concept_class_id"]] %||% "" %>% as.character()
    ) %>%
      dplyr::mutate(
        dplyr::across(.data$conceptName:.data$conceptClassId, ~tidyr::replace_na(.x, "")) #convert na to ""
      )
    conceptList <- purrr::pmap(conceptDf, newConcept)
  }

  rlang::inject(cs(!!!conceptList, name = name, id = id))
}

# Other ----

#' Fill in Concept Set details using a vocab
#'
#' Concept sets created in R using the `cs` function do not contain details like
#' "CONCEPT_NAME", "DOMAIN_ID", etc. If an OMOP CDM vocabulary is available then
#' these details can be filled in by the the `getConceptSetDetails` function.
#'
#' @param x A concept set created by `cs`
#' @param con A connection to an OMOP CDM database
#' @param vocabularyDatabaseSchema   Schema name where your OMOP vocabulary format resides. Note that
#'                                   for SQL Server, this should include both the database and schema
#'                                   name, for example 'vocabulary.dbo'.
#' @return A modified version of the input concept set with concept details filled in.
#'
#' @importFrom methods slot<-
#' @export
#'
#' @examples
#' \dontrun{
#' # create a concept set
#' vocabularyDatabaseSchema = "cdm5"
#' anemia <- cs(descendants(439777,4013073,4013074))
#'
#' # fill in the details from an OMOP CDM
#' library(DatabaseConnector)
#' con <- connect(dbms = "postgresql", user = "postgres", password = "", server = "localhost/covid")
#' anemia <- getConceptSetDetails(condition_anemia, con, vocabularyDatabaseSchema = "cdm5")
#' }
getConceptSetDetails <- function(x,
                                con,
                                vocabularyDatabaseSchema = NULL) {

  checkmate::assertClass(x, "ConceptSet")
  checkmate::assertTRUE(DBI::dbIsValid(con))
  checkmate::assertCharacter(vocabularyDatabaseSchema, len = 1, null.ok = TRUE)


  ids <- purrr::map_int(x@Expression, ~.@Concept@concept_id)

  sql <- "SELECT * FROM @schema.concept WHERE concept_id IN (@ids);" %>%
    SqlRender::render(schema = vocabularyDatabaseSchema, ids = ids) %>%
    SqlRender::translate(targetDialect = DatabaseConnector::dbms(con))

  df <- DBI::dbGetQuery(con, sql) %>%
    tibble::tibble() %>%
    dplyr::rename_all(tolower) %>%
    # TODO what is the logic is for filling in the caption and invalid_reason fields?
    dplyr::mutate(invalid_reason = ifelse(
      is.na(.data$invalid_reason), "V", .data$invalid_reason)
      ) %>%
    dplyr::mutate(
      standard_concept_caption = dplyr::case_when(
        standard_concept == "S" ~ "Standard",
        standard_concept == "" ~ "Non-Standard",
        standard_concept == "C" ~ "Classification",
        TRUE ~ "")) %>%
    dplyr::mutate(invalid_reason_caption = dplyr::case_when(
        invalid_reason == "V" ~ "Valid",
        invalid_reason == "I" ~ "Invalid",
        TRUE ~ "")
    )

  checkSlotNames <- methods::slotNames("Concept")[-1]

  for (i in seq_along(x@Expression)) {
    id <- x@Expression[[i]]@Concept@concept_id
    for (n in checkSlotNames) {
      dtl <- dplyr::filter(df, .data$concept_id == id) %>%
        dplyr::pull(!!n)
      if (length(dtl > 0)) {
        methods::slot(x@Expression[[i]]@Concept, n) <- dtl
      }
    }
  }
  return(x)
}

#' FUnction checks if two concept set class objects are equivalent
#' @name ==
#' @param e1,e2 a ConceptSet Class object
#' @aliases ==,ConceptSet,ConceptSet-method
#' @docType methods
#' @rdname equals-methods
#' @export
setMethod("==", signature = c(e1 = "ConceptSet", e2 = "ConceptSet"), function(e1, e2) {

  lhs <- as.data.frame(e1) %>% dplyr::arrange(.data$conceptId)
  rhs <- as.data.frame(e2) %>% dplyr::arrange(.data$conceptId)
  isTRUE(all.equal(lhs, rhs))
})

# args(getGeneric("unique")) # get generic argument names

# TODO convert this to vctrs. use generic.
uniqueConceptSets <- function(x) {
  stopifnot(is.list(x), all(purrr::map_lgl(x, ~methods::is(., "ConceptSet"))))
  # Is there an efficient implementation using just equality? Seems like maybe not?
  l <- list()
  for (i in x) {
    alreadyIn <- any(purrr::map_lgl(l, ~i == .))
    if(!alreadyIn) {
      l <- c(l, i)
    }
  }
  return(l)
}

# numberConceptSets <- function(x) {
#   stopifnot(is.list(x), all(purrr::map_lgl(x, ~is(., "ConceptSet"))))
#   conceptSetList <- list()
#   conceptSetIds <- list(0L = x@)
#   for (i in seq_along(x)) {
#
#     alreadyIn <- any(purrr::map_lgl(l, ~i == .))
#     if(!alreadyIn) {
#       l <- c(l, i)
#     }
#   }
#   return(l)
# }

# Build Capr call ----

#' Create the Capr code to build a concept set
#'
#' @param x A concept set
#' @param name the name of the concept set
#' @return The Capr code required to build the concept set
#'
getConceptSetCall <- function(x, name = x@Name){

  checkmate::assertCharacter(name, len = 1, min.chars = 1)

  df <- as.data.frame(x)

  # TODO improve formatting for multi-lines. Possibly make this code more concise?
  csCall <- ""
  ids <- df[df$includeDescendants & df$isExcluded & df$includeMapped,]$conceptId
  if (length(ids) > 0) {
    csCall <- c(paste0("exclude(desendants(mapped(", paste0(ids, collapse = ", "), ")))"), csCall)
  }

  ids <- df[df$includeDescendants & df$isExcluded & !df$includeMapped,]$conceptId
  if (length(ids) > 0) {
    csCall <- c(paste0("exclude(desendants(", paste0(ids, collapse = ", "), "))"), csCall)
  }

  ids <- df[df$includeDescendants & !df$isExcluded & df$includeMapped,]$conceptId
  if (length(ids) > 0) {
    csCall <- c(paste0("desendants(mapped(", paste0(ids, collapse = ", "), "))"), csCall)
  }

  ids <- df[!df$includeDescendants & df$isExcluded & df$includeMapped,]$conceptId
  if (length(ids) > 0) {
    csCall <- c(paste0("exclude(mapped(", paste0(ids, collapse = ", "), "))"), csCall)
  }

  ids <- df[!df$includeDescendants & !df$isExcluded & df$includeMapped,]$conceptId
  if (length(ids) > 0) {
    csCall <- c(paste0("mapped(", paste0(ids, collapse = ", "), ")"), csCall)
  }

  ids <- df[!df$includeDescendants & df$isExcluded & !df$includeMapped,]$conceptId
  if (length(ids) > 0) {
    csCall <- c(paste0("exclude(", paste0(ids, collapse = ", "), ")"), csCall)
  }

  ids <- df[df$includeDescendants & !df$isExcluded & !df$includeMapped,]$conceptId
  if (length(ids) > 0) {
    csCall <- c(paste0("descendants(", paste0(ids, collapse = ", "), ")"), csCall)
  }

  ids <- df[!df$includeDescendants & !df$isExcluded & !df$includeMapped,]$conceptId
  if (length(ids) > 0) {
    csCall <- c(paste0(ids, collapse = ", "), csCall)
  }

  paste0(name, " <- cs(", paste(csCall[csCall != ""], collapse = ", "), ")")
}

# getConceptSetCall(cs(1,2, exclude(4,3)), "blah")

# create a lookup table mapping guids to codesetIds
dedupConceptSets <- function(conceptSetList) {
  uniqueConceptSets <- list(conceptSetList[[1]])
  lookup <- c(0) %>% rlang::set_names(conceptSetList[[1]]@id)
  i <- 1
  for (newCS in conceptSetList[-1]) {
    alreadyIn <- FALSE
    for (existingCS in uniqueConceptSets) {
      if (newCS == existingCS) {
        lookup[newCS@id] <- lookup[existingCS@id]
        alreadyIn <- TRUE
        break
      }
    }
    if (!alreadyIn) {
      lookup[newCS@id] <- i
      uniqueConceptSets <- c(uniqueConceptSets, newCS)
      i <- i + 1
    }
  }
  return(list(lookup = lookup, uniqueConceptSets = uniqueConceptSets))
}

# r <- dedupConceptSets(list(cs(2,1), cs(2,3), cs(1,2)))
