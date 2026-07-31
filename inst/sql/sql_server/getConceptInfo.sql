-- getConceptInfo.sql
-- Retrieves full concept table details for a list of concept IDs.
-- Includes validity dates and invalid_reason so callers can detect deprecated concepts.
--
-- Parameters:
--   @schema     : vocabulary database schema
--   @conceptIds : comma-separated integer concept IDs

SELECT
    concept_id,
    concept_name,
    domain_id,
    vocabulary_id,
    concept_class_id,
    standard_concept,
    concept_code,
    valid_start_date,
    valid_end_date,
    invalid_reason
FROM @schema.concept
WHERE concept_id IN (@conceptIds)
ORDER BY concept_name;
