WITH agg AS 
(
SELECT 

z.geocodigo::double precision AS geocodigo,
c.nom_comuna,

-- INDICADOR 1: Porcentaje de Población Migrante (Adultos).
-- Fórmula: (Total de migrantes > 18 años / Total de personas > 18 años) * 100
ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99) AND p.p09 > 18) * 100.0 / 
    NULLIF(COUNT(*) FILTER (WHERE p.p09 > 18), 0), 2
    ) AS ptje_migrantes_adultos,
    
-- INDICADOR 2: Porcentaje de Migrantes ADULTOS con Escolaridad Incompleta
-- Fórmula: (Total de migrantes > 18 con escolaridad < 8 / Total de migrantes > 18) * 100
ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99) AND p.p15 < 8 AND p.p09 > 18) * 100.0 / 
    NULLIF(COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99) AND p.p09 > 18), 0), 2
    ) AS ptje_esc_inc_mig_adultos

FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna
JOIN public.provincias AS pr ON pr.provincia_ref_id = c.provincia_ref_id
WHERE (pr.nom_provincia = 'SANTIAGO' OR c.nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO'))
GROUP BY z.geocodigo, c.nom_comuna
)
SELECT a.geocodigo,
    shp.geom,
    a.nom_comuna,
    a.ptje_migrantes_adultos,
    a.ptje_esc_inc_mig_adultos
FROM agg AS a
JOIN dpa.zonas_censales_rm AS shp ON shp.geocodigo = a.geocodigo
WHERE shp.urbano = 1;