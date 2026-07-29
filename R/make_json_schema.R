library(jsonlite)

date_time_pattern <- "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"

index_schema <- list(
  `$schema` = "https://json-schema.org/draft/2020-12/schema",
  `$id` = "https://github.com/NINAnor/ecRxiv/schema/index-schema.json",
  title = "Ecological condition assessment import",
  type = "object",
  additionalProperties = FALSE,
  
  required = c("input", "output"),
  
  properties = list(
    input = list(
      `$ref` = "#/$defs/input"
    ),
    output = list(
      `$ref` = "#/$defs/output"
    )
  ),
  
  `$defs` = list(
    
    input = list(
      type = "object",
      additionalProperties = FALSE,
      required = c("kode", "datasett"),
      properties = list(
        kode = list(
          type = "array",
          items = list(`$ref` = "#/$defs/kode")
        ),
        datasett = list(
          type = "array",
          items = list(`$ref` = "#/$defs/datasett")
        )
      )
    ),
    
    output = list(
      type = "object",
      additionalProperties = FALSE,
      required = c(
        "indikatorVurderinger",
        "rapportData"
      ),
      properties = list(
        indikatorVurderinger = list(
          type = "array",
          items = list(
            `$ref` = "#/$defs/indikatorVurdering"
          )
        ),
        rapportData = list(
          `$ref` = "#/$defs/rapportData"
        )
      )
    ),
    
    kode = list(
      type = "object",
      additionalProperties = FALSE,
      required = c("navn", "link"),
      properties = list(
        navn = list(type = "string", minLength = 1),
        link = list(type = "string", format = "uri")
      )
    ),
    
    datasett = list(
      type = "object",
      additionalProperties = FALSE,
      required = c(
        "navn",
        "link",
        "kilde",
        "periodeStart",
        "periodeSlutt",
        "type"
      ),
      properties = list(
        navn = list(type = "string", minLength = 1),
        link = list(type = "string", format = "uri"),
        kilde = list(type = "string", minLength = 1),
        periodeStart = list(
          type = "string",
          pattern = date_time_pattern
        ),
        periodeSlutt = list(
          type = "string",
          pattern = date_time_pattern
        ),
        type = list(type = "string", minLength = 1)
      )
    ),
    
    indikatorVurdering = list(
      type = "object",
      additionalProperties = FALSE,
      required = c(
        "indikatorBeskrivelse",
        "indikatorReferanseUid",
        "geografiskOmradeReferanseUid",
        "nedreKonfidensIntervalGrense",
        "tilstandsverdi",
        "ovreKonfidensIntervalGrense",
        "periodeStart",
        "periodeSlutt"
      ),
      properties = list(
        indikatorBeskrivelse = list(
          type = "string",
          minLength = 1
        ),
        indikatorReferanseUid = list(
          type = "string",
          minLength = 1
        ),
        geografiskOmradeReferanseUid = list(
          `$ref` = "#/$defs/geografiskOmradeReferanseUid"
        ),
        nedreKonfidensIntervalGrense = list(
          type = "number"
        ),
        tilstandsverdi = list(
          type = "number"
        ),
        ovreKonfidensIntervalGrense = list(
          type = "number"
        ),
        periodeStart = list(
          type = "string",
          pattern = date_time_pattern
        ),
        periodeSlutt = list(
          type = "string",
          pattern = date_time_pattern
        )
      )
    ),
    
    rapportData = list(
      type = "object",
      additionalProperties = FALSE,
      required = c(
        "navn",
        "totalVurdering",
        "egenskapsVurderinger",
        "indikatorVurderinger_egenskapVurderinger_vekting"
      ),
      properties = list(
        navn = list(
          type = "string",
          minLength = 1
        ),
        totalVurdering = list(
          type = "array",
          items = list(
            `$ref` = "#/$defs/totalVurdering"
          )
        ),
        egenskapsVurderinger = list(
          type = "array",
          items = list(
            `$ref` = "#/$defs/egenskapsVurdering"
          )
        ),
        indikatorVurderinger_egenskapVurderinger_vekting= list(
          type = "array",
          items = list(
            `$ref` = "#/$defs/vekting"
      )
    )
  )
)
)
)

dir.create("config", showWarnings = FALSE)

jsonlite::write_json(
  index_schema,
  path = "config/index-schema.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)