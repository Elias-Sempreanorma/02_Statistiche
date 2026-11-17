# ---------------------------------------------------------------------------
# Immagini disponibili e coordinate dei punti interattivi.
# Le coordinate sono espresse inizialmente in pixel rispetto all'immagine
# originale e poi convertite in percentuale, cosi' restano corrette anche
# quando l'immagine si ridimensiona.
# ---------------------------------------------------------------------------
immagini_macchine <- data.frame(
  coupon = c(
    "LIN-SEMP-3625-7560-4998-4816-219",
    "LIN-SEMP-0943-0891-7413-6940-252",
    "LIN-SEMP-4354-8784-4143-4591-177",
    "LIN-SEMP-3106-8575-8743-0135-225",
    "LIN-SEMP-8335-4662-1347-7995-125",
    "MAC-SEMP-7908-7476-8539-6940-087",
    "MAC-SEMP-9512-3712-9471-1352-029",
    "LIN-SEMP-1711-6111-9357-7313-217",
    "LIN-SEMP-6943-2521-9960-7175-136",
    "LIN-SEMP-3357-9670-4875-9907-093",
    "LIN-SEMP-3167-2891-4825-4255-052",
    "LIN-SEMP-0831-2914-0355-6989-227"
  ),
  file_name = c(
    "Giacomini_G1_A3020.png",
    "Giacomini_G1_A1220.png",
    "Giacomini_G1_C14GR.png",
    "Giacomini_G1_E11RI.png",
    "Giacomini_G1_F0400.png",
    "Siboni_Banco_1.png",
    "Siboni_Banco_2.png",
    "Zorzetto_Pelatrice_3.png",
    "Zorzetto_Trafila_2.png",
    "Giacomini_G3_ST21.png",
    "Calpeda_Linea_L4.png",
    "Zorzetto_Trafila_3.png"
  ),
  stringsAsFactors = FALSE
)

mappa_sensori <- bind_rows(
  data.frame(
    coupon = "LIN-SEMP-3625-7560-4998-4816-219", image_width = 964, image_height = 345,
    cds_name = c(
      "EML", "REG", "PE2", "BIM2", "FCM5", "FCM6", "FCM4",
      "FCM3", "FCM2", "FCM1", "FCM7", "FCM8", "FCM13", "PE1",
      "FCM9", "FCM12", "FCM11", "FCM10", "BIM1"
    ),
    x = c(
      536, 556, 96, 86, 129, 144, 159, 324, 341, 372,
      62, 62, 473, 457, 371, 172, 324, 341, 169
    ),
    y = c(
      29, 78, 121, 142, 161, 161, 160, 171, 170, 188,
      200, 223, 231, 236, 252, 255, 268, 268, 277
    )
  ),
  data.frame(
    coupon = "LIN-SEMP-0943-0891-7413-6940-252", image_width = 898, image_height = 325,
    cds_name = c(
      "EML", "REG", "FCM5", "FCM4", "FCM3", "FCM2", "FCM1",
      "FCM7", "FCM8", "FCM12", "FCM11", "FCM10", "FCM9",
      "FCM14", "FCM15", "PE1", "FCM13", "BIM1 - A", "BIM2 - A"
    ),
    x = c(
      485, 505, 93, 108, 273, 290, 321,
      11, 11, 100, 273, 290, 320,
      355, 387, 406, 422, 90, 65
    ),
    y = c(
      32, 81, 164, 163, 174, 173, 191,
      203, 226, 254, 271, 271, 255,
      242, 236, 239, 234, 304, 137
    )
  ),
  data.frame(
    coupon = "LIN-SEMP-4354-8784-4143-4591-177", image_width = 774, image_height = 642,
    cds_name = c(
      "EML", "REG", "MB4", "MB3", "SM3", "MB2", "SM2", "SC1",
      "SM1", "FCM1", "MB1", "PE1", "RE1", "MB5", "MB6", "SM4"
    ),
    x = c(
      662, 701, 346, 370, 439, 444, 548, 581, 747, 704,
      740, 448, 539, 345, 375, 275
    ),
    y = c(
      35, 35, 132, 133, 122, 200, 171, 219, 374, 408,
      416, 438, 437, 455, 454, 441
    )
  ),
  data.frame(
    coupon = "LIN-SEMP-3106-8575-8743-0135-225", image_width = 790, image_height = 508,
    cds_name = c(
      "MF1", "MF2", "FCM6", "FCM5", "RA2", "FCM7", "RA6",
      "FCM4", "UPe2", "MBe2", "RA5", "FCM3", "FCM2", "FCM1",
      "MBe1", "UPe1", "RA4", "RA7", "UPe3", "MB4", "MB3",
      "FCM9", "FTCe1", "UPe4", "FCM8", "FMEe3", "RA3", "RA1",
      "FMEe1", "MB1", "MB2", "FMEe2", "EML", "REG"
    ),
    x = c(
      60, 168, 402, 461, 265, 310, 444, 554, 499, 498,
      498, 554, 461, 402, 436, 453, 423, 365, 405, 226,
      246, 300, 323, 365, 310, 282, 199, 177, 60, 99,
      139, 166, 719, 749
    ),
    y = c(
      36, 36, 46, 46, 99, 129, 100, 139, 137, 155,
      194, 217, 290, 290, 214, 239, 224, 130, 105, 163,
      183, 158, 178, 178, 217, 142, 213, 250, 222, 222,
      222, 214, 349, 378
    )
  ),
  data.frame(
    coupon = "LIN-SEMP-8335-4662-1347-7995-125", image_width = 932, image_height = 678,
    cds_name = c(
      "EML", "REG", "MB6", "MB5", "MB4", "MB3", "MB2", "SM3",
      "SM2", "R3", "PE1", "FCM1", "FTC1", "MB1", "SM1"
    ),
    x = c(
      302, 263, 412, 234, 234, 595, 595, 223, 598, 576,
      233, 180, 535, 502, 602
    ),
    y = c(
      97, 97, 173, 195, 216, 194, 212, 278, 278, 381,
      386, 419, 435, 484, 462
    )
  ),
  data.frame(
    coupon = "MAC-SEMP-7908-7476-8539-6940-087", image_width = 1293, image_height = 1217,
    cds_name = c("FCM1", "FCM2", "MB1"),
    x = c(884, 982, 673),
    y = c(94, 94, 405)
  ),
  data.frame(
    coupon = "MAC-SEMP-9512-3712-9471-1352-029", image_width = 1293, image_height = 1217,
    cds_name = c("FCM1", "FCM2", "MB1"),
    x = c(884, 982, 673),
    y = c(94, 94, 405)
  ),
  data.frame(
    coupon = "LIN-SEMP-1711-6111-9357-7313-217", image_width = 877, image_height = 596,
    cds_name = c("FCM1", "FCM2", "FCM3", "MB1", "MB2"),
    x = c(294, 390, 365, 275, 715),
    y = c(175, 458, 555, 330, 202)
  ),
  data.frame(
    coupon = "LIN-SEMP-6943-2521-9960-7175-136", image_width = 1375, image_height = 376,
    cds_name = c("FTC1", "FTC5", "FTC6", "FCM1", "FCM2", "FCM3", "FCM4", "SC1", "SC3"),
    x = c(150, 1014, 1014, 192, 341, 384, 496, 285, 1344),
    y = c(83, 229, 161, 304, 179, 183, 178, 112, 166)
  ),
  # ST21: PCA escluso dalla mappa come richiesto.
  # CHIAVE e RESET hanno hotspot distinti sul selettore e sul pulsante blu.
  data.frame(
    coupon = "LIN-SEMP-3357-9670-4875-9907-093", image_width = 913, image_height = 558,
    cds_name = c(
      "EML", "FCM1", "FCM10", "FCM12", "FCM3", "FCM4", "MB2", "MB6", "PE1", "PE2", "RA2 (CHIAVE)", "RA2 (RESET)", "RA3 (CHIAVE)", "RA3 (RESET)", "RA4 (CHIAVE)", "RA4 (RESET)", "RA5 (CHIAVE)", "RA5 (RESET)", "REG", "SC1", "FCM2", "MB4"
    ),
    x = c(443, 153, 791, 479, 279, 279, 762, 481, 570, 853, 789, 789, 733, 733, 459, 459, 450, 450, 443, 516, 231, 521),
    y = c(527, 339, 396, 249, 368, 315, 117, 182, 186, 391, 114, 97, 448, 433, 409, 392, 178, 163, 498, 343, 302, 444)
  ),
  # BIM1 sulla pulsantiera P1, BIM2 sulla pulsantiera P2.
  # Le due meta' di ogni pulsantiera distinguono IN e AV.
  data.frame(
    coupon = "LIN-SEMP-3167-2891-4825-4255-052",
    image_width = 801, image_height = 340,
    cds_name = c("FCM 1", "FCM2", "BIM 1 - IN", "BIM 1 - AV", "BIM2 - IN", "BIM 2 - AV"),
    x = c(235, 284, 259, 269, 337, 347),
    y = c(230, 230, 124, 124, 144, 144)
  ),
  # Trafila 3: solo componenti identificabili nel disegno.
  # FTC1, FTC2 e FTC3 sono ancorati alle rispettive scritte.
  data.frame(
    coupon = "LIN-SEMP-0831-2914-0355-6989-227",
    image_width = 945, image_height = 370,
    cds_name = c(
      "FTC1", "FCM5", "SC1", "SC2", "PE1", "PE2", "FTC2", "FTC3",
      "FCM3", "FCM4", "RE1", "RE2", "RE5", "RE4", "RE7"
    ),
    x = c(80, 289, 609, 923, 272, 473, 173, 428, 326, 286, 150, 161, 512, 385, 932),
    y = c(17, 70, 64, 131, 148, 150, 230, 152, 115, 100, 21, 177, 148, 148, 119)
  )
) |>
  mutate(
    cds_key = str_to_upper(str_squish(cds_name)),
    x_pct = 100 * x / image_width,
    y_pct = 100 * y / image_height
  )


