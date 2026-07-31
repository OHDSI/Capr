-- fastSearchConcepts.sql (spark)
--
-- Covers Databricks and Apache Spark SQL.
-- Uses levenshtein() for similarity: normalized to 0-1 as
--   1 - levenshtein(a, b) / GREATEST(LENGTH(a), LENGTH(b), 1)
-- RLIKE handles word-boundary boost. Delta Lake auto-optimizes LIKE scans.
-- Synonym scan is scoped to matched IDs only.
--
-- Parameters:
--   @schema          : vocabulary database schema (catalog.schema for Unity Catalog)
--   @keyword         : raw search term
--   @domainFilter    : optional AND clause for domain_id (built in R, "" if unused)
--   @standardFilter  : optional AND clause for standard_concept (built in R, "" if unused)
--   @limit           : max rows returned
--   @offset          : pagination offset

WITH name_matched AS (
    SELECT concept_id
    FROM @schema.concept
    WHERE LOWER(concept_name) LIKE LOWER('%@keyword%')
      AND invalid_reason IS NULL
    @domainFilter
    @standardFilter
    LIMIT 500
),
code_matched AS (
    SELECT concept_id
    FROM @schema.concept
    WHERE LOWER(concept_code) LIKE LOWER('%@keyword%')
      AND invalid_reason IS NULL
    LIMIT 200
),
matched_ids AS (
    SELECT concept_id FROM name_matched
    UNION
    SELECT concept_id FROM code_matched
),
syn_scores AS (
    SELECT concept_id,
           MAX(1.0 - levenshtein(LOWER(concept_synonym_name), LOWER('@keyword'))
                     / GREATEST(LENGTH(concept_synonym_name), LENGTH('@keyword'), 1)) AS max_syn_sim
    FROM @schema.concept_synonym
    WHERE concept_id IN (SELECT concept_id FROM matched_ids)
    GROUP BY concept_id
),
scored AS (
    SELECT
        c.concept_id,
        c.concept_name,
        c.concept_code,
        c.vocabulary_id,
        c.domain_id,
        c.concept_class_id,
        c.standard_concept,
        GREATEST(
            1.0 - levenshtein(LOWER(c.concept_name), LOWER('@keyword'))
                  / GREATEST(LENGTH(c.concept_name), LENGTH('@keyword'), 1),
            COALESCE(
                1.0 - levenshtein(LOWER(c.concept_code), LOWER('@keyword'))
                      / GREATEST(LENGTH(c.concept_code), LENGTH('@keyword'), 1),
                0),
            COALESCE(s.max_syn_sim, 0)
        )
        + CASE
            WHEN LOWER(c.concept_name) = LOWER('@keyword')     THEN 0.5
            WHEN LOWER(c.concept_name) LIKE LOWER('@keyword%') THEN 0.3
            WHEN c.concept_name RLIKE CONCAT('(?i)\\b@keyword') THEN 0.15
            ELSE 0
          END
        + COALESCE(map_counts.mapping_count, 0) * 0.01        AS relevance,
        COALESCE(map_counts.mapping_count, 0)                 AS mapping_count
    FROM matched_ids mi
    JOIN @schema.concept c ON mi.concept_id = c.concept_id
    LEFT JOIN syn_scores s ON c.concept_id = s.concept_id
    LEFT JOIN (
        SELECT concept_id_1 AS concept_id,
               COUNT(*)     AS mapping_count
        FROM @schema.concept_relationship
        WHERE relationship_id = 'Maps to'
          AND invalid_reason IS NULL
          AND concept_id_1 IN (SELECT concept_id FROM matched_ids)
        GROUP BY concept_id_1
    ) map_counts ON c.concept_id = map_counts.concept_id
    WHERE c.invalid_reason IS NULL
    @domainFilter
    @standardFilter
)
SELECT concept_id, concept_name, concept_code, vocabulary_id,
       domain_id, concept_class_id, standard_concept, relevance, mapping_count
FROM scored
ORDER BY relevance DESC
LIMIT @limit
OFFSET @offset;
