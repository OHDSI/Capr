-- searchConcepts.sql
-- Searches the OMOP concept table by keyword using a case-insensitive LIKE match.
-- Returns standard or all concepts filtered optionally by domain.
--
-- Parameters:
--   @schema           : vocabulary database schema (e.g. "cdm.dbo"); pass "" for no schema prefix
--   @keyword          : search term; matched as LOWER(concept_name) LIKE '%keyword%'
--   @domainFilter     : optional AND clause for domain_id (built in R, "" if no filter)
--   @standardFilter   : optional AND standard_concept = 'S' clause (built in R, "" if no filter)
--   @limit            : maximum rows to return

SELECT TOP @limit
    concept_id,
    concept_name,
    domain_id,
    vocabulary_id,
    concept_class_id,
    standard_concept,
    concept_code
FROM @schema.concept
WHERE LOWER(concept_name) LIKE LOWER('%@keyword%')
  AND invalid_reason IS NULL
  @domainFilter
  @standardFilter
ORDER BY concept_name;
