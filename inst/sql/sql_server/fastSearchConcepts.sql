-- fastSearchConcepts.sql (sql_server / universal fallback)
--
-- Two-phase approach: ILIKE narrows candidates, then boost-ranking scores them.
-- No dialect-specific similarity function; uses positional boost only.
-- Synonym and mapping-count enrichment are scoped to matched IDs for efficiency.
--
-- Parameters:
--   @schema          : vocabulary database schema
--   @keyword         : raw search term (used both as LIKE pattern and for boost checks)
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
),
code_matched AS (
    SELECT concept_id
    FROM @schema.concept
    WHERE LOWER(concept_code) LIKE LOWER('%@keyword%')
      AND invalid_reason IS NULL
),
matched_ids AS (
    SELECT concept_id FROM name_matched
    UNION
    SELECT concept_id FROM code_matched
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
        CASE
            WHEN LOWER(c.concept_name) = LOWER('@keyword')        THEN 1.0
            WHEN LOWER(c.concept_name) LIKE LOWER('@keyword%')    THEN 0.7
            WHEN LOWER(c.concept_name) LIKE LOWER('%@keyword%')   THEN 0.4
            ELSE 0.2
        END
        + COALESCE(map_counts.mapping_count, 0) * 0.01 AS relevance,
        COALESCE(map_counts.mapping_count, 0)           AS mapping_count
    FROM matched_ids mi
    JOIN @schema.concept c ON mi.concept_id = c.concept_id
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
OFFSET @offset ROWS
FETCH NEXT @limit ROWS ONLY;
