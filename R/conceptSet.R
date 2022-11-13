
# Concept set related classes ------------------------------------------

#' An S4 class for a single OMOP Concept
#'
#' A concept class contains all the information about the concept from the OMOP voabulary.
#'
#' @slot concept_id the OMOP/OHDSI concept ID
#' @slot concept_name the name of the concept
#' @slot standard_concept whether the concept is standard 'S', classification 'C', or  non-standard NA
#' @slot standrad_concept_caption Whether the concept is standard full phrase
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

#' @rdname show-method
#' @aliases show,Concept-method
setMethod("show", "Concept", function(object) {
  nm <- methods::slotNames(methods::is(object))
  concept <- unname(sapply(nm, slot, object = object))
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
#' @rdname show-method
#' @aliases show,ConceptSetItem-method
#' @export
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
         slots = c(id = "character",
                   Name = "character",
                   Expression = "list"))

setValidity("ConceptSet", function(object) {
  stopifnot(is.character(object@id),
            length(object@id) == 1,
            is.character(object@id),
            length(object@id) == 1,
            is.list(object@Expression),
            all(purrr::map_lgl(object@Expression, ~is(., "ConceptSetItem")))
            )
  TRUE
})

#' @rdname show-method
#' @aliases show,ConceptSet-method
setMethod("show", "ConceptSet", function(object) {
  cat(paste("<Capr Concept Set>", object@Name, "\n"))
  cat("Id:", object@id,"\n")
  d <- purrr::map_chr(object@Expression, ~.@Concept@concept_id)
  print(as.data.frame(object))
})

# Constructors ------------------------
# User facing concept set constructors
# From the user's perspective there are only concept sets with zero or more members.

# Internal
newConcept <- function(id, isExcluded = FALSE, includeDescendants = FALSE, includeMapped = FALSE) {
  checkmate::assertIntegerish(id, len = 1)

  new("ConceptSetItem",
      Concept = new("Concept", concept_id = as.integer(id)),
      isExcluded = isExcluded,
      includeDescendants = includeDescendants,
      includeMapped = includeMapped)
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
    } else if (is(x, "ConceptSetItem") || is.null(x)) {
      return(x)
    }
  })

  ids <- purrr::map_int(conceptList, ~.@Concept@concept_id)
  dups <- ids[duplicated(ids)]
  if (length(dups) > 0)
    rlang::abort(paste("ID: ", paste(dups, collapse = ", "), " are duplicated in the concept set."))
  # TODO decide how to handle duplicate ids in `cs`

  if (is.null(id)) id <- as.character(uuid::UUIDgenerate())

  methods::new("ConceptSet",
               id = id,
               Name = name,
               Expression = conceptList)
}

# Concept set helpers -----

#' Exclude concepts from a concept set
#'
#' `exclude` is meant to be used inside `cs` when creating a new concept set.
#'
#' @param ... One or more numeric vectors that can be coerced to integers, or
#' Calls to helper functions "exclude", "descendants", or "mapped".
#'
#' @return A list of Capr concepts
#' @export
#' @describeIn cs
exclude <- function(...) {
  dots <- unlist(list(...), recursive = F)

  lapply(dots, function(x) {
    if (is.numeric(x) && length(x) == 1) {
      return(newConcept(x, isExcluded = TRUE))
    } else if (is(x, "ConceptSetItem")) {
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
#' @describeIn cs
mapped <- function(...) {
  dots <- unlist(list(...), recursive = F)

  lapply(dots, function(x) {
    if (is.numeric(x) && length(x) == 1) {
      return(newConcept(x, includeMapped = TRUE))
    } else if (is(x, "ConceptSetItem")) {
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
#' @describeIn cs
descendants <- function(...) {
  dots <- unlist(list(...), recursive = F)

  lapply(dots, function(x) {
    if (is.numeric(x) && length(x) == 1) {
      return(newConcept(x, includeDescendants = TRUE))
    } else if (is(x, "ConceptSetItem")) {
      x@includeDescendants <- TRUE
      return(x)
    }
  })
}

#' Coerce a concept set expression to a dataframe
#'
#' @param x A Caper Concept Set
#'
#' @return A tibble (dataframe) with columns: concept_id, includeDescendants, isExcluded, includeMapped.
#' @export
as.data.frame.ConceptSet <- function(x) {
  tibble::tibble(
    conceptId = purrr::map_int(x@Expression, ~.@Concept@concept_id),
    includeDescendants = purrr::map_lgl(x@Expression, "includeDescendants"),
    isExcluded = purrr::map_lgl(x@Expression, "isExcluded"),
    includeMapped = purrr::map_lgl(x@Expression, "includeMapped")
  )
}

setMethod("as.list", "Concept", function(x){
  nm <- methods::slotNames(methods::is(x))
  concept <- lapply(nm, slot, object = x)
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
                 'Name' = x@Name,
                 'Expression' = lapply(x@Expression, as.list))
})

#' Save a concept set as a json file
#'
#' The resulting concept Set JSON file can be imported into Atlas.
#'
#' @param x A Capr concept set created by `cs()`
#' @param path Name of file to write to. (e.g. "concepts.json")
#'
#' @export
#'
#' @examples
#' \dontrun{
#' anemia <- cs(descendants(439777,4013073,4013074))
#' writeConceptSet(anemia, 'anemia.json')
#' }
writeConceptSet <- function(x, path) {
  items <- list(items = lapply(x@Expression, as.list))
  jsonlite::write_json(x = items, path = path, pretty = TRUE, auto_unbox = TRUE)
}

# condition_anemia <- cs(descendants(439777,4013073,4013074))
# jsonlite::write_json()
# x <- condition_anemia


#' Fill in Concept Set details using a vocab
#'
#' Concept sets created in R using the `cs` function do not contain details like
#' "CONCEPT_NAME", "DOMAIN_ID", etc. If an OMOP CDM vocabulary is available then
#' these details can be filled in by the the `getConceptSetDetails` function.
#'
#' @param x A concept set created by `cs`
#' @param con A connection to an OMOP CDM database
#' @template VocabularyDatabaseSchema
#' @return A modified version of the input concept set with concept details filled in.
#'
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
#'}
getConceptSetDetails <- function(x,
                                con,
                                vocabularyDatabaseSchema = NULL) {

  checkmate::assertClass(x, "ConceptSet")
  checkmate::assertTRUE(DBI::dbIsValid(con))
  checkmate::assertCharacter(vocabularyDatabaseSchema, len = 1, null.ok = TRUE)


  ids <- purrr::map_int(x@Expression, ~.@Concept@concept_id)

  sql <- "SELECT * FROM @schema.concept WHERE concept_id IN (@ids);" %>%
    SqlRender::render(schema = vocabularyDatabaseSchema, ids = ids) %>%
    SqlRender::translate(targetDialect = dbms(con))

  df <- DBI::dbGetQuery(con, sql) %>%
    tibble::tibble() %>%
    dplyr::rename_all(tolower) %>%
    # Not exactly sure what the logic is for filling in the captions and invalid_reason
    dplyr::mutate(invalid_reason = ifelse(is.na(invalid_reason), "V", invalid_reason)) %>%
    dplyr::mutate(
      standard_concept_caption = case_when(
        standard_concept == "S" ~ "Standard",
        standard_concept == "" ~ "Non-Standard",
        standard_concept == "C" ~ "Classification",
        TRUE ~ "")) %>%
    dplyr::mutate(invalid_reason_caption = case_when(
        invalid_reason == "V" ~ "Valid",
        invalid_reason == "I" ~ "Invalid",
        TRUE ~ "")
    )

  for (i in seq_along(x@Expression)) {
    id <- x@Expression[[i]]@Concept@concept_id
    for (n in slotNames("Concept")[-1]) {
      dtl <- dplyr::filter(df, .data$concept_id == .env$id) %>% dplyr::pull(!!n)
      if (length(dtl > 0)) slot(x@Expression[[i]]@Concept, n) <- dtl
    }
  }
  return(x)
}

