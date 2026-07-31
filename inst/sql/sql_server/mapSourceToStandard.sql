-- mapSourceToStandard.sql
-- Maps non-standard (source) concept codes to their standard OMOP equivalents
-- via the concept_relationship 'Maps to' relationship.
-- Returns one row per source-code/standard-concept pair.
--
-- Parameters:
--   @schema      : vocabulary database schema
--   @sourceCodes : comma-separated quoted source codes (e.g. 'I48','I48.0')
--   @vocabFilter : optional AND clause restricting source vocabulary_id (built in R, "" if no filter)

SELECT
    sc.concept_id       AS source_concept_id,
    sc.concept_name     AS source_concept_name,
    sc.concept_code     AS source_code,
    sc.vocabulary_id    AS source_vocabulary_id,
    tc.concept_id       AS standard_concept_id,
    tc.concept_name     AS standard_concept_name,
    tc.domain_id        AS standard_domain_id,
    tc.vocabulary_id    AS standard_vocabulary_id
FROM @schema.concept sc
JOIN @schema.concept_relationship cr
    ON  sc.concept_id       = cr.concept_id_1
    AND cr.relationship_id  = 'Maps to'
    AND cr.invalid_reason   IS NULL
JOIN @schema.concept tc
    ON  cr.concept_id_2     = tc.concept_id
    AND tc.standard_concept = 'S'
WHERE UPPER(sc.concept_code) IN (@sourceCodes)
  AND sc.invalid_reason IS NULL
  @vocabFilter
ORDER BY sc.concept_code, tc.concept_name;
