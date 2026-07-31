-- getConceptDescendants.sql
-- Traverses concept_ancestor to return all descendants of one or more seed concepts.
-- Seed concepts themselves are included when @minLevels = 0.
--
-- Parameters:
--   @schema    : vocabulary database schema
--   @conceptIds: comma-separated integer ancestor concept IDs
--   @minLevels : minimum levels of separation from the ancestor (0 includes the seed itself)
--   @maxLevels : maximum levels of separation (use a large number such as 99999 for all)

SELECT
    c.concept_id,
    c.concept_name,
    c.domain_id,
    c.vocabulary_id,
    c.concept_class_id,
    c.standard_concept,
    c.concept_code,
    ca.min_levels_of_separation,
    ca.max_levels_of_separation
FROM @schema.concept_ancestor ca
JOIN @schema.concept c
    ON ca.descendant_concept_id = c.concept_id
WHERE ca.ancestor_concept_id IN (@conceptIds)
  AND ca.min_levels_of_separation >= @minLevels
  AND ca.min_levels_of_separation <= @maxLevels
  AND c.invalid_reason IS NULL
ORDER BY ca.min_levels_of_separation, c.concept_name;
