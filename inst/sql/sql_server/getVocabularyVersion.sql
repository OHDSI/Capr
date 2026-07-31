/*
   getVocabularyVersion.sql
   Retrieves the OMOP vocabulary version from the vocabulary table.
   The row where vocabulary_id = 'None' stores the full vocabulary release string.

   Parameters:
     @schema : vocabulary database schema
*/

SELECT vocabulary_version
FROM @schema.vocabulary
WHERE vocabulary_id = 'None';
