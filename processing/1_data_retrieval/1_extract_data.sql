---Run for each year ([pub_year])-----

--Dimensions reference pairs
SELECT DISTINCT p.citing_pub_id, p.cited_pub_id
  FROM [dimensions_2024jul].[dbo].[pub] as d
  left join dimensions_2024jul.dbo.citation as p on d.pub_id=p.citing_pub_id
  WHERE d.[pub_year] = 2023 AND 
		d.[doi] IS NOT NULL AND
    d.pub_type_id IN (1, 6)


--Language of cited references
SELECT DISTINCT p.cited_pub_id, oa.language_iso2_code, d2.pub_type_id
  FROM [dimensions_2024jul].[dbo].[pub] as d
  LEFT JOIN dimensions_2024jul.dbo.citation as p ON d.pub_id = p.citing_pub_id
  LEFT JOIN dimensions_2024jul.dbo.pub as d2 ON p.cited_pub_id = d2.pub_id
  LEFT JOIN [openalex_2024aug].[dbo].[work] as oa ON d2.doi = oa.doi
  WHERE d.[pub_year] = 2023 AND 
    d.[doi] IS NOT NULL AND
    d.pub_type_id IN (1, 6) AND
    d2.[doi] IS NOT NULL
  
--Language of citing document
SELECT DISTINCT p.citing_pub_id, oa.language_iso2_code
  FROM [dimensions_2024jul].[dbo].[pub] as d
  left join dimensions_2024jul.dbo.citation as p on d.pub_id=p.citing_pub_id
  LEFT JOIN [openalex_2024aug].[dbo].[work] as oa ON d.doi = oa.doi
  WHERE d.[pub_year] = 2023 AND 
    d.[doi] IS NOT NULL AND
    d.pub_type_id IN (1, 6)


--Language of all publications (with and without citation links)
SELECT DISTINCT d.pub_id, oa.language_iso2_code
  FROM [dimensions_2024jul].[dbo].[pub] as d
  LEFT JOIN [openalex_2024aug].[dbo].[work] as oa ON d.doi = oa.doi
  WHERE d.[pub_year] = 2023 AND 
    d.[doi] IS NOT NULL AND
    d.pub_type_id IN (1, 6) AND
	oa.language_iso2_code IS NOT NULL


----------Journal level data

SELECT p.[source_id]
  ,p.[pub_id]
FROM [dimensions_2024jul].[dbo].pub as p
WHERE p.[doi] IS NOT NULL AND
  p.pub_type_id = 1 AND
  p.[pub_year] < 2024 AND p.[pub_year] >= 1990 

----------Discipline data

SELECT p.Pub_ID, d.for_division_id
FROM dimensions_2024jul.dbo.pub AS p
INNER JOIN dimensions_2024jul.dbo.pub_for_division AS d ON p.pub_id = d.pub_id
WHERE
        p.pub_type_id IN (1, 6)
        AND p.pub_year = 2023


---Author data
select p.Pub_ID, au.author_seq, au.author_id
FROM dimensions_2024jul.dbo.pub as p
inner join [dimensions_2024jul].dbo.pub_author as au on p.pub_id=au.pub_id
WHERE
        p.pub_type_id IN (1, 6)
        AND p.pub_year = 2023

select pub_id, author_seq, MIN(affiliation_seq) as affiliation_seq
into [userdb_larivierevg].dbo.simplified_pub_author_affiliation
from dimensions_2024jul.dbo.pub_author_affiliation
GROUP BY pub_id, author_seq

select distinct au.author_id,pac.country_code
FROM dimensions_2024jul.dbo.pub as p
inner join [userdb_larivierevg].dbo.extract_data_missing_ids as id on au.author_id=id.author_id ---aux table with author ids
inner join [dimensions_2024jul].dbo.pub_author as au on p.pub_id=au.pub_id
left join dimensions_2024jul.dbo.author as aut on au.author_id=aut.author_id
left join [userdb_larivierevg].dbo.simplified_pub_author_affiliation as aff on p.pub_id = aff.pub_id and au.author_seq=aff.author_seq
left join dimensions_2024jul.dbo.pub_affiliation_country as pac on pac.pub_id=p.pub_id  and pac.affiliation_seq=aff.affiliation_seq
WHERE ISNUMERIC(id.author_id) = 1 AND id.author_id <> 'NA'
  
