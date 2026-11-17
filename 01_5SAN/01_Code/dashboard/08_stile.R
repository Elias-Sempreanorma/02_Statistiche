dashboard_stile <- function() {
    tags$style(HTML("
      /* Titolo dashboard: header blu originale, scritta e simbolo grigio chiaro. */
      .san-dashboard-header {
        margin: 0 -15px;
        height: 93px;
        box-sizing: border-box;
        padding: 8px 22px;
        background: linear-gradient(115deg, #234A66 10%, #3B5D77 72%, #5F748C);
        border-bottom: 2px solid var(--san-yellow);
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 20px;
        min-height: 0;
      }
      .san-dashboard-title-img {
        display: block;
        width: auto;
        height: 105px;
        max-width: min(76%, 1200px);
        flex-shrink: 1;
        object-fit: contain;
        object-position: left center;
      }
      .san-dashboard-logo {
        height: 70px;
        width: auto;
        flex-shrink: 0;
        object-fit: contain;
      }
      @media (max-width: 700px) {
        .san-dashboard-header { height: 70px; min-height: 0; padding: 6px 16px; }
        .san-dashboard-title-img { height: 66px; max-width: 70%; }
        .san-dashboard-logo { height: 48px; }
      }
      /* Modal grande: quasi a schermo intero */
      .modal-xl {
        width: 95vw;
        max-width: 95vw;
      }
      .modal-xl .modal-body {
        max-height: 82vh;
        overflow-y: auto;
      }

      #nok_table table,
      #modal_nok_table table {
        width: 100%;
        border-collapse: collapse;
      }
      #nok_table th,
      #modal_nok_table th {
        background-color: #E9EDF1;
        color: #4A4A4A;
        font-weight: 600;
        text-align: center !important;
        padding: 8px 10px;
        border-bottom: 2px solid #D5DBE0;
      }
      #nok_table td,
      #modal_nok_table td {
        text-align: center !important;
        padding: 10px 12px;
        border-bottom: 1px solid #E4E7EB;
        color: #234A66;
        font-weight: 700;
        font-size: 18px;
      }
      .pannello-filtri {
        background-color: #F4F6F8;
        border-radius: 6px;
        padding: 8px 14px 0 14px;
        margin-bottom: 8px;
      }
      .pannello-filtri .form-group {
        margin-bottom: 8px;
      }
      .pannello-filtri label {
        margin-bottom: 3px;
        font-size: 12px;
      }
      .pannello-filtri .form-control {
        height: 32px;
        padding: 4px 8px;
        font-size: 13px;
      }
      .pannello-filtri .input-group-addon {
        padding: 4px 8px;
        font-size: 12px;
      }
      .titolo-sezione {
        color: #4A4A4A;
        font-weight: 600;
        margin-top: 10px;
        margin-bottom: 10px;
      }
      .activation-scroll {
        width: 100%;
        overflow-x: auto;
        overflow-y: hidden;
        padding-bottom: 8px;
      }
      .activation-scroll-inner {
        display: block;
      }
      .documenti-bar {
        display: flex;
        align-items: center;
        gap: 14px;
        flex-wrap: wrap;
        margin: 0 0 10px 0;
        padding: 9px 13px;
        border: 1px solid #D9DDE0;
        border-radius: 7px;
        background: #F7F7F7;
      }
      .documenti-title {
        display: flex;
        align-items: center;
        gap: 7px;
        color: #234A66;
        font-size: 13px;
        font-weight: 700;
      }
      .documenti-links {
        display: flex;
        align-items: center;
        gap: 8px;
        flex-wrap: wrap;
      }
      .documento-download {
        display: inline-block;
        padding: 5px 9px;
        border: 1px solid #CDD2D6;
        border-radius: 6px;
        background: #FFFFFF;
        color: #234A66 !important;
        font-size: 12px;
        font-weight: 600;
        text-decoration: none !important;
      }
      .documento-download:hover,
      .documento-download:focus {
        background: #F1F2F3;
        color: #234A66 !important;
      }
      .modal-documenti {
        padding-top: 2px;
      }
      .modal-documenti-title {
        display: flex;
        align-items: center;
        gap: 7px;
        margin-bottom: 6px;
        color: #234A66;
        font-size: 12px;
        font-weight: 700;
      }
      .modal-documenti .documenti-links {
        display: flex;
        align-items: flex-start;
        gap: 6px;
        flex-wrap: wrap;
      }
      .card-home {
        display: block;
        width: 100%;
        min-height: 104px;
        background: #FFFFFF;
        border: 1px solid #E4E7EB;
        border-radius: 9px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.06);
        transition: transform 0.15s ease, box-shadow 0.15s ease, border-color 0.15s ease;
        text-align: left;
        white-space: normal;
        padding: 12px 13px;
        margin-bottom: 10px;
        color: #4A4A4A;
      }
      .card-home:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 18px rgba(0,0,0,0.10);
        border-color: #5F748C;
      }
      .card-home .card-icona {
        font-size: 20px;
        color: #5F748C;
        margin-bottom: 4px;
      }
      .card-home h4 {
        font-size: 15px;
        font-weight: 700;
        margin: 0 0 4px 0;
      }
      .card-home p {
        font-size: 11.5px;
        line-height: 1.3;
        color: #8A94A0;
        margin: 0;
      }
      .schema-home {
        width: 100%;
        display: flex;
        justify-content: center;
        margin: 0;
      }
      .schema-frame {
        position: relative;
        width: 100%;
        max-width: 100%;
      }
      .schema-frame img {
        display: block;
        width: 100%;
        height: auto;
        border: 2px solid #8F9AA5;
        border-radius: 7px;
        background: #FFFFFF;
        box-shadow: 0 1px 4px rgba(36,54,75,0.12);
      }
      .sensor-hotspot {
        position: absolute;
        width: 24px;
        height: 24px;
        transform: translate(-50%, -50%);
        border: 2px solid transparent;
        border-radius: 50%;
        background: transparent;
        box-shadow: none;
        cursor: pointer;
        z-index: 2;
        outline: none;
      }
      .sensor-hotspot:hover,
      .sensor-hotspot:focus {
        background: rgba(127, 166, 201, 0.58);
        border-color: #234A66;
        z-index: 20;
      }
      .sensor-tooltip {
        display: none;
        position: absolute;
        left: 50%;
        bottom: calc(100% + 10px);
        transform: translateX(-50%);
        min-width: 220px;
        padding: 10px 12px;
        border-radius: 7px;
        background: #234A66;
        color: #FFFFFF;
        text-align: left;
        font-size: 13px;
        line-height: 1.5;
        white-space: nowrap;
        box-shadow: 0 4px 12px rgba(0,0,0,0.25);
        pointer-events: none;
      }
      .sensor-tooltip::after {
        content: '';
        position: absolute;
        top: 100%;
        left: 50%;
        margin-left: -6px;
        border-width: 6px;
        border-style: solid;
        border-color: #234A66 transparent transparent transparent;
      }
      .sensor-hotspot:hover .sensor-tooltip,
      .sensor-hotspot:focus .sensor-tooltip {
        display: block;
      }
      .sensor-tooltip-title {
        display: block;
        margin-bottom: 4px;
        font-weight: 700;
      }
      .home-menu {
        padding-right: 18px;
      }
      .home-stage {
        position: relative;
        width: 100%;
        height: clamp(600px, calc(100vh - 190px), 760px);
        min-height: 600px;
        margin: 0 0 12px 0;
        overflow: hidden;
        background: #F4F5F6;
      }

      /* Schema grande, centrato e sopra ai quattro pulsanti. */
      .home-schema-layer {
        position: absolute;
        inset: 0;
        display: flex;
        align-items: center;
        justify-content: center;
        z-index: 5;
        pointer-events: none;
      }
      .home-schema-layer .schema-home {
        width: 100%;
        margin: 0;
        display: flex;
        justify-content: center;
        pointer-events: none;
      }
      .home-schema-layer .schema-frame {
        width: min(84vw, 1500px);
        max-width: calc(100% - 180px);
        pointer-events: auto;
      }

      /* Quattro pulsanti rettangolari: ognuno occupa un quarto della pagina. */
      .home-corner {
        position: absolute;
        z-index: 1;
        width: 50%;
        height: 50%;
      }
      .home-corner-tl {
        top: 0;
        left: 0;
      }
      .home-corner-tr {
        top: 0;
        right: 0;
      }
      .home-corner-bl {
        bottom: 0;
        left: 0;
      }
      .home-corner-br {
        right: 0;
        bottom: 0;
      }

      .home-corner .card-home {
        position: relative;
        width: 100%;
        height: 100%;
        min-height: 0;
        margin: 0;
        padding: 0;
        border: 0;
        border-radius: 0;
        background: #F4F5F6;
        box-shadow: none;
        text-align: left;
        transition: background-color 0.15s ease;
      }
      .home-corner .card-home:hover {
        transform: none;
        background: #E7E0C8;
        border: 0;
        box-shadow: inset 0 0 0 2px rgba(161, 148, 103, 0.12);
      }
      .home-corner-tl .card-home {
        border-right: 2px solid #CDD2D6;
        border-bottom: 2px solid #CDD2D6;
      }
      .home-corner-tr .card-home {
        border-bottom: 2px solid #CDD2D6;
      }
      .home-corner-bl .card-home {
        border-right: 2px solid #CDD2D6;
      }

      /* Descrizione visibile nell'angolo esterno del relativo rettangolo. */
      .home-corner .card-home .home-card-label {
        position: absolute;
        width: clamp(180px, 12vw, 215px);
        min-height: 86px;
        padding: 13px 15px;
        border: 0;
        border-radius: 9px;
        background: #FFFFFF;
        box-shadow: none;
        z-index: 2;
      }
      .home-corner .card-home:hover .home-card-label {
        background: #F3F4F5;
        box-shadow: none;
      }
      .home-corner .card-home h4 {
        font-size: 17px;
        font-weight: 700;
        margin: 0 0 5px 0;
        color: #234A66;
      }
      .home-corner .card-home p {
        font-size: 12.5px;
        line-height: 1.35;
        margin: 0;
        color: #68737E;
      }
      .home-corner-tl .home-card-label {
        top: 12px;
        left: 12px;
      }
      .home-corner-tr .home-card-label {
        top: 12px;
        right: 12px;
      }
      .home-corner-bl .home-card-label {
        bottom: 12px;
        left: 12px;
      }
      .home-corner-br .home-card-label {
        right: 12px;
        bottom: 12px;
      }

      .alarm-section {
        margin: 0 0 28px 0;
        padding: 0 0 24px 0;
        border-bottom: 1px solid #E4E7EB;
      }
      .alarm-section:last-child {
        border-bottom: 0;
        margin-bottom: 0;
        padding-bottom: 0;
      }
      .alarm-empty {
        margin-top: 10px;
        padding: 12px 14px;
        border: 1px solid #DDE4EA;
        border-radius: 7px;
        background: #F8FAFB;
        color: #5F6F7F;
        font-size: 13px;
      }
      .report-panel {
        margin: 0 0 28px 0;
        padding: 18px 20px;
        border: 1px solid #D9DDE0;
        border-radius: 12px;
        background: linear-gradient(135deg, #FBFAF6 0%, #F1F3F4 100%);
        box-shadow: 0 3px 12px rgba(36,54,75,0.06);
      }
      .report-buttons {
        display: flex;
        flex-wrap: wrap;
        gap: 9px;
        margin-top: 10px;
      }
      .report-period-btn {
        border: 1px solid #CDD2D6;
        border-radius: 999px;
        background: #FFFFFF;
        color: #234A66;
        font-size: 12px;
        font-weight: 700;
        padding: 8px 13px;
        cursor: pointer;
        box-shadow: 0 1px 3px rgba(36,54,75,0.05);
        transition: background 0.15s ease, transform 0.15s ease;
      }
      .report-period-btn:hover {
        background: #F1F2F3;
        transform: translateY(-1px);
      }
      .report-modal {
        color: #334155;
        padding-bottom: 18px;
        font-family: 'Segoe UI', 'Inter', 'Helvetica Neue', Arial, sans-serif;
        letter-spacing: -0.005em;
      }
      .report-hero {
        margin: -15px -15px 22px -15px;
        padding: 25px 28px 24px 28px;
        background: linear-gradient(135deg, #234A66 0%, #354C65 100%);
        color: #FFFFFF;
        border-radius: 8px 8px 14px 14px;
        box-shadow: 0 7px 20px rgba(36,54,75,0.16);
      }
      .report-modal-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 22px;
      }
      .report-heading-left {
        display: flex;
        align-items: flex-start;
        gap: 13px;
        min-width: 0;
      }
      .report-back-btn.btn {
        flex-shrink: 0;
        width: 38px;
        height: 38px;
        margin-top: 1px;
        padding: 0 !important;
        border: 1px solid rgba(255,255,255,0.30) !important;
        border-radius: 50% !important;
        background: rgba(255,255,255,0.08) !important;
        color: #FFFFFF !important;
        font-size: 16px;
        box-shadow: none !important;
      }
      .report-back-btn.btn:hover,
      .report-back-btn.btn:focus {
        background: rgba(255,255,255,0.18) !important;
        color: #FFFFFF !important;
        outline: none !important;
      }
      .report-title {
        margin: 0;
        color: #FFFFFF;
        font-size: 32px;
        line-height: 1.12;
        font-weight: 780;
        letter-spacing: -0.025em;
      }
      .report-subtitle {
        margin-top: 10px;
        color: #E6EDF3;
        font-size: 15px;
        line-height: 1.42;
      }
      .report-download.btn {
        flex-shrink: 0;
        margin-top: 2px;
        padding: 9px 14px;
        border: 1px solid rgba(255,255,255,0.36) !important;
        border-radius: 8px !important;
        background: rgba(255,255,255,0.10) !important;
        color: #FFFFFF !important;
        font-weight: 700;
      }
      .report-download.btn:hover {
        background: rgba(255,255,255,0.18) !important;
      }
      .report-meta {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        margin-top: 15px;
      }
      .report-meta-chip {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 6px 10px;
        border: 1px solid rgba(255,255,255,0.18);
        border-radius: 999px;
        background: rgba(255,255,255,0.07);
        color: #EEF3F7;
        font-size: 12px;
        font-weight: 650;
      }
      .report-kpi-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0,1fr));
        gap: 11px;
        margin: 0 0 22px 0;
      }
      .report-kpi-card {
        min-width: 0;
        padding: 14px 15px;
        border: 1px solid #E0E5EA;
        border-radius: 10px;
        background: #FFFFFF;
        box-shadow: 0 2px 8px rgba(36,54,75,0.05);
      }
      .report-kpi-label {
        color: #718096;
        font-size: 10px;
        font-weight: 800;
        letter-spacing: 0.07em;
        text-transform: uppercase;
      }
      .report-kpi-value {
        margin-top: 5px;
        color: #234A66;
        font-size: 23px;
        line-height: 1.12;
        font-weight: 800;
      }
      .report-kpi-note {
        margin-top: 4px;
        color: #718096;
        font-size: 11px;
        line-height: 1.35;
      }
      .report-summary {
        margin: 0 0 24px 0;
        padding: 20px 22px;
        border: 1px solid #D9DDE0;
        border-left: 5px solid #EBBD55;
        border-radius: 10px;
        background: #F7F7F7;
        color: #334155;
        line-height: 1.58;
        font-size: 15px;
      }
      .report-summary-title {
        margin: 0 0 12px 0;
        color: #234A66;
        font-size: 20px;
        font-weight: 800;
        letter-spacing: -0.015em;
      }
      .report-summary p {
        margin: 0 0 12px 0;
      }
      .report-summary p:last-child {
        margin-bottom: 0;
      }
      .report-section {
        margin: 0 0 28px 0;
      }
      .report-section-heading {
        display: flex;
        align-items: center;
        gap: 9px;
        margin: 0 0 10px 0;
        padding-bottom: 8px;
        border-bottom: 1px solid #E4E7EB;
      }
      .report-section-number {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 24px;
        height: 24px;
        border-radius: 50%;
        background: #234A66;
        color: #FFFFFF;
        font-size: 11px;
        font-weight: 800;
      }
      .report-section-title {
        margin: 0;
        color: #234A66;
        font-size: 16px;
        font-weight: 800;
      }
      .report-section-note {
        margin: -3px 0 10px 33px;
        color: #718096;
        font-size: 11px;
      }
      .report-table-wrap {
        width: 100%;
        overflow-x: auto;
        border: 1px solid #E0E5EA;
        border-radius: 9px;
        background: #FFFFFF;
      }
      .report-table {
        width: 100%;
        border-collapse: collapse;
        margin: 0;
        font-size: 12px;
      }
      .report-table th {
        background: #EDF1F4;
        color: #234A66;
        border-bottom: 1px solid #D8E0E6;
        border-right: 1px solid #E1E6EA;
        padding: 9px 10px;
        text-align: center;
        white-space: nowrap;
      }
      .report-table td {
        border-bottom: 1px solid #EEF1F4;
        border-right: 1px solid #EEF1F4;
        padding: 9px 10px;
        text-align: center;
        vertical-align: top;
        white-space: nowrap;
      }
      .report-table tbody tr:nth-child(even) {
        background: #FAFBFC;
      }
      .report-table th:last-child,
      .report-table td:last-child {
        border-right: 0;
      }
      .report-ok {
        color: #19764A;
        font-weight: 800;
      }
      .report-bad {
        color: #B42318;
        font-weight: 800;
      }
      .report-nd {
        color: #718096;
        font-weight: 700;
      }
      .report-status-pill {
        display: inline-block;
        margin-left: 5px;
        padding: 2px 7px;
        border-radius: 999px;
        font-size: 9px;
        font-weight: 800;
        text-transform: uppercase;
      }
      .report-status-ok {
        color: #19764A;
        background: #DDF3E6;
      }
      .report-status-bad {
        color: #A93B32;
        background: #F7D9D5;
      }
      @media (max-width: 900px) {
        .report-kpi-grid {
          grid-template-columns: repeat(2, minmax(0,1fr));
        }
        .report-modal-header {
          display: block;
        }
        .report-download.btn {
          margin-top: 14px;
        }
      }
      .modal-filters {
        background: #F4F6F8;
        border-radius: 7px;
        padding: 14px 16px 4px 16px;
        margin-bottom: 18px;
      }
      .modal-data-button {
        margin: 4px 0 10px 0;
        text-align: right;
      }
      .modal-data-panel {
        margin: 0 0 22px 0;
        padding: 12px;
        border: 1px solid #E4E7EB;
        border-radius: 7px;
        background: #FFFFFF;
      }
      .modal-machine-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 16px;
        margin: 0 0 12px 0;
        padding: 2px 0 10px 0;
      }
      .modal-machine-title {
        color: #234A66;
        font-size: 18px;
        font-weight: 700;
        line-height: 1.25;
      }
      .modal-close-x {
        flex-shrink: 0;
        border: 0;
        background: transparent;
        color: #4A4A4A;
        font-size: 31px;
        font-weight: 400;
        line-height: 1;
        padding: 0 5px;
        cursor: pointer;
        opacity: 0.72;
      }
      .modal-close-x:hover,
      .modal-close-x:focus {
        color: #234A66;
        opacity: 1;
        outline: none;
      }
      .modal-machine-toolbar {
        display: flex; align-items: center; justify-content: space-between;
        gap: 16px; flex-wrap: wrap; margin-bottom: 20px;
      }
      .modal-machine-toolbar > .modal-nav-bar { margin-bottom: 0; }
      .machine-guide-open { margin-left: auto; border-radius: 12px; color: #234A66; }
      .machine-details-content { position: relative; }
      .machine-details-content > .machine-layout-thumbs {
        position: absolute; right: 22px; top: 18px; z-index: 31;
        display: flex; gap: 8px; max-width: 190px; overflow-x: auto; padding: 4px;
      }
      .machine-details-content:has(> .machine-layout-thumbs) .modal-filters {
        padding-right: 235px; min-height: 128px;
      }
      #machine-guide-dialog {
        width: min(1120px, 94vw); max-width: 94vw; max-height: 90dvh;
        padding: 0; border: 0; border-radius: 22px; overflow: hidden;
        box-shadow: 0 20px 70px rgba(0,0,0,.35);
      }
      #machine-guide-dialog::backdrop { background: rgba(18,35,49,.55); }
      #machine-guide-dialog .san-guide { max-height: 88dvh; }
      @media (max-width: 700px) {
        .machine-details-content > .machine-layout-thumbs {
          position: static; max-width: 100%; justify-content: flex-end; margin-bottom: 10px;
        }
        .machine-details-content:has(> .machine-layout-thumbs) .modal-filters { padding-right: 12px; }
      }
      .machine-layout-thumb {
        flex: 0 0 auto; border: 1px solid #CCD6DE; border-radius: 10px;
        background: white; padding: 4px 8px; cursor: zoom-in;
      }
      .machine-layout-thumb img { width: 150px; height: 80px; object-fit: contain; display: block; }
      .machine-layout-thumb:focus-visible { outline: 3px solid #EDBE48; outline-offset: 2px; }
      #machine-layout-dialog {
        width: min(1200px, 94vw); max-width: 94vw; max-height: 90vh;
        padding: 16px; border: 1px solid #CCD6DE; border-radius: 16px;
        box-shadow: 0 20px 70px rgba(0,0,0,.35); overflow: auto;
      }
      #machine-layout-dialog::backdrop { background: rgba(18,35,49,.55); }
      .machine-layout-heading { display: flex; align-items: center; justify-content: space-between; gap: 16px; margin-bottom: 12px; }
      .machine-layout-heading strong { color: #234A66; font-size: 18px; }
      .machine-layout-close { border: 1px solid #CCD6DE; border-radius: 8px; background: white; padding: 6px 12px; }
      #machine-layout-dialog img { display: block; width: 100%; height: auto; }
      .modal-nav-bar {
        margin-bottom: 20px;
        padding-bottom: 14px;
        border-bottom: 2px solid #E4E7EB;
      }
      .modal-nav-bar .btn-group .btn {
        font-weight: 700;
        font-size: 14px;
        padding: 9px 19px;
        color: #234A66 !important;
        background: #FFFFFF !important;
        border-color: #D6DADF !important;
        box-shadow: none !important;
      }
      .modal-nav-bar .btn-group .btn:hover,
      .modal-nav-bar .btn-group .btn:focus {
        color: #234A66 !important;
        background: #F3F4F5 !important;
        border-color: #EBBD55 !important;
      }
      .modal-nav-bar .btn-group .btn.active,
      .modal-nav-bar .btn-group .btn.active:hover,
      .modal-nav-bar .btn-group .btn.active:focus {
        color: #234A66 !important;
        background: #EBBD55 !important;
        border-color: #D4A23B !important;
        box-shadow: inset 0 1px 3px rgba(36,54,75,0.12) !important;
      }

      /* ------------------------------------------------------------------
         Vita componenti - card con valore corrente, massimo, utilizzo e residuo
         ------------------------------------------------------------------ */
      .life-header {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 18px;
        margin: 4px 0 18px 0;
      }
      .life-header-title {
        margin: 0;
        color: #234A66;
        font-size: 23px;
        font-weight: 700;
      }
      .life-header-subtitle {
        margin-top: 4px;
        color: #718096;
        font-size: 14px;
      }
      .life-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 16px;
        width: 100%;
      }
      .life-card {
        min-width: 0;
        background: #FFFFFF;
        border: 1px solid #DDE4EA;
        border-radius: 10px;
        padding: 15px 17px 14px 17px;
        box-shadow: 0 2px 7px rgba(0,0,0,0.045);
      }
      .life-card-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 10px;
        padding-bottom: 10px;
        margin-bottom: 13px;
        border-bottom: 1px solid #E6EBEF;
      }
      .life-card-title {
        min-width: 0;
        color: #234A66;
        font-size: 14px;
        line-height: 1.25;
        font-weight: 700;
      }
      .life-status {
        flex-shrink: 0;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        border-radius: 999px;
        padding: 4px 10px;
        font-size: 11px;
        font-weight: 700;
      }
      .life-status-ok {
        color: #19764A;
        background: #DDF3E6;
      }
      .life-status-warning {
        color: #996400;
        background: #FFF0C8;
      }
      .life-status-over {
        color: #A93B32;
        background: #F7D9D5;
      }
      .life-row {
        margin-bottom: 16px;
      }
      .life-row:last-child {
        margin-bottom: 1px;
      }
      .life-row + .life-row {
        padding-top: 13px;
        border-top: 1px solid #EEF1F4;
      }
      .life-row-header {
        display: flex;
        justify-content: space-between;
        align-items: baseline;
        gap: 10px;
        margin-bottom: 7px;
      }
      .life-metric {
        flex-shrink: 0;
        color: #234A66;
        font-size: 13px;
        font-weight: 700;
      }
      .life-value {
        color: #234A66;
        font-size: 12px;
        font-weight: 600;
        text-align: right;
        white-space: nowrap;
      }
      .life-progress {
        position: relative;
        width: 100%;
        height: 14px;
        overflow: visible;
        background: #E4E9ED;
        border-radius: 999px;
      }
      .life-progress-fill {
        height: 100%;
        border-radius: 999px;
        background: #77B98D;
      }
      .life-progress-fill.warning {
        background: #E1B552;
      }
      .life-progress-fill.over {
        background: #D9897F;
      }
      .life-progress-marker {
        position: absolute;
        top: 50%;
        width: 10px;
        height: 10px;
        transform: translate(-50%, -50%);
        border-radius: 50%;
        background: #259762;
        box-shadow: 0 0 0 2px #FFFFFF;
      }
      .life-progress-marker.warning {
        background: #C58A17;
      }
      .life-progress-marker.over {
        background: #BB4F45;
      }
      .life-row-footer {
        display: flex;
        justify-content: space-between;
        gap: 10px;
        margin-top: 6px;
        color: #718096;
        font-size: 11px;
      }
      .life-row-footer strong {
        color: #25865B;
      }
      .life-row-footer strong.warning {
        color: #A16C08;
      }
      .life-row-footer strong.over {
        color: #A93B32;
      }
      @media (max-width: 1250px) {
        .life-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }
      @media (max-width: 760px) {
        .life-grid {
          grid-template-columns: 1fr;
        }
        .life-header {
          display: block;
        }
        .life-value {
          white-space: normal;
        }
      }

      @media (max-width: 1050px) {
        .home-stage {
          height: 620px;
          min-height: 620px;
        }
        .home-schema-layer .schema-frame {
          max-width: calc(100% - 170px);
        }
        .home-corner .card-home .home-card-label {
          width: 132px;
          min-height: 70px;
          padding: 9px 10px;
        }
      }

      @media (max-width: 700px) {
        .home-menu {
          padding-right: 15px;
        }
        .home-stage {
          display: grid;
          grid-template-columns: 1fr;
          gap: 12px;
          height: auto;
          min-height: 0;
          overflow: visible;
        }
        .home-schema-layer,
        .home-corner {
          position: static;
          width: 100%;
          height: auto;
        }
        .home-schema-layer {
          order: 1;
          pointer-events: auto;
        }
        .home-schema-layer .schema-home {
          pointer-events: auto;
        }
        .home-schema-layer .schema-frame {
          width: 100% !important;
          max-width: 100%;
        }
        .home-corner-tl {
          order: 2;
        }
        .home-corner-tr {
          order: 3;
        }
        .home-corner-bl {
          order: 4;
        }
        .home-corner-br {
          order: 5;
        }
        .home-corner .card-home {
          width: 100%;
          height: auto;
          min-height: 82px;
          padding: 10px 12px;
          border-radius: 9px;
        }
        .home-corner .card-home .home-card-label {
          position: static;
          width: 100%;
          min-height: 0;
          padding: 0;
          border: 0;
          border-radius: 0;
          background: transparent;
          box-shadow: none;
        }
        .sensor-hotspot {
          width: 18px;
          height: 18px;
        }
        .sensor-tooltip {
          min-width: 190px;
          font-size: 12px;
          white-space: normal;
        }
      }

      /* ==============================================================
         TEMA SEMPREANORMA
         Layout invariato: si interviene solo su colori, tipografia,
         bordi, hover e gerarchia visiva.
         ============================================================== */
      :root {
        --san-blue: #234A66;
        --san-blue-2: #5F748C;
        --san-yellow: #EBBD55;
        --san-yellow-dark: #D4A23B;
        --san-bg: #F4F5F6;
        --san-panel: #FFFFFF;
        --san-panel-soft: #F0F2F3;
        --san-border: #C5CDD3;
        --san-border-soft: #DDE1E4;
        --san-text: #202326;
        --san-muted: #66717A;
      }

      body,
      .form-control,
      .btn,
      .dropdown-menu,
      .modal-content,
      .selectize-input,
      .selectize-dropdown {
        font-family: Arial, 'Helvetica Neue', Helvetica, sans-serif;
      }

      body {
        color: var(--san-text);
        background: linear-gradient(145deg, #F4F5F6, #EDF1F4);
      }

      /* Fascia alta: il blu resta concentrato qui, come nel sito. */
      .container-fluid > h2:first-of-type {
        margin: 0 -15px;
        padding: 13px 20px 12px 20px;
        background: linear-gradient(120deg, #234A66, #42647D);
        border-bottom: 2px solid var(--san-yellow);
        color: #FFFFFF;
        font-size: 29px;
        font-weight: 500;
        letter-spacing: -0.02em;
      }

      .pannello-filtri {
        position: relative;
        z-index: 30;
        overflow: visible;
        margin: 0 -7px 8px -7px;
        padding: 10px 14px 1px 14px;
        background: linear-gradient(135deg, rgba(255,255,255,0.88), rgba(242,247,251,0.70));
        border: 1px solid rgba(255,255,255,0.90);
        border-top: 0;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 8px 24px rgba(35,74,102,0.06), inset 0 1px 0 rgba(255,255,255,0.95);
      }
      .pannello-filtri label,
      .modal-filters label {
        color: var(--san-blue);
        font-weight: 700;
      }
      .pannello-filtri .form-control,
      .modal-filters .form-control,
      .bootstrap-select > .dropdown-toggle {
        background: #FFFFFF;
        border-color: #C8CFD4;
        color: var(--san-text);
        box-shadow: none;
      }
      .pannello-filtri .form-control:focus,
      .modal-filters .form-control:focus,
      .bootstrap-select.open > .dropdown-toggle {
        border-color: var(--san-blue-2);
        box-shadow: 0 0 0 2px rgba(95,116,140,0.10);
      }

      input[type='radio'],
      input[type='checkbox'] {
        accent-color: var(--san-yellow-dark);
      }

      /* Vetro riservato ai controlli: niente superfici blur sovrapposte. */
      .pannello-filtri {
        -webkit-backdrop-filter: blur(12px) saturate(115%);
        backdrop-filter: blur(12px) saturate(115%);
      }
      .home-stage {
        z-index: 0;
        isolation: isolate;
        background: linear-gradient(135deg, #EDF1F4, #F4F5F6);
        border: 1px solid rgba(255,255,255,0.85);
        border-radius: 24px;
        box-shadow: 0 12px 32px rgba(35,74,102,0.08);
      }
      .home-corner .card-home {
        background-color: #F4F5F6;
        background-image: linear-gradient(145deg, rgba(255,255,255,0.72), rgba(255,255,255,0.08));
        border: 0 !important;
        border-radius: 22px;
        box-shadow: inset 0 0 0 1px rgba(197,205,211,0.65), inset 0 2px 0 rgba(255,255,255,0.90);
        transition: background-color 0.22s ease, box-shadow 0.22s ease;
      }
      .home-corner .card-home:hover,
      .home-corner .card-home:focus-visible {
        background-color: var(--san-yellow);
        background-image: linear-gradient(145deg, rgba(255,255,255,0.38), rgba(255,255,255,0.02));
        box-shadow: inset 0 0 0 1px rgba(255,255,255,0.95), inset 0 2px 0 #FFFFFF, 0 8px 24px rgba(35,74,102,0.08);
        outline: none;
      }
      .home-corner .card-home:focus-visible {
        outline: 3px solid var(--san-blue);
        outline-offset: -6px;
      }

      .home-corner .card-home .home-card-label {
        background: transparent;
        border: 1px solid transparent;
        border-radius: 18px;
        box-shadow: none;
        transition: color 0.22s ease;
      }
      .home-corner .card-home:hover .home-card-label,
      .home-corner .card-home:focus-visible .home-card-label {
        background: transparent;
        border-color: transparent;
        box-shadow: none;
      }
      .home-corner .card-home h4 {
        color: var(--san-blue);
        font-weight: 700;
      }
      .home-corner .card-home p {
        color: var(--san-muted);
      }

      .schema-frame img {
        border: 1.5px solid rgba(95,116,140,0.50);
        border-radius: 20px;
        background: #FFFFFF;
        box-shadow: 0 16px 40px rgba(35,74,102,0.12), 0 0 0 4px rgba(255,255,255,0.65);
      }

      /* Hotspot: blu in hover, bordo giallo come accento. */
      .sensor-hotspot:hover,
      .sensor-hotspot:focus {
        background: rgba(95,116,140,0.26);
        border-color: var(--san-yellow);
      }
      .sensor-tooltip {
        background: var(--san-blue);
        border-left: 3px solid var(--san-yellow);
      }
      .sensor-tooltip::after {
        border-color: var(--san-blue) transparent transparent transparent;
      }

      /* Modali: testata blu, contenuto bianco/grigio, selezione gialla. */
      .modal-content {
        background: linear-gradient(135deg, rgba(255,255,255,0.96), rgba(242,247,251,0.92));
        border: 1px solid rgba(255,255,255,0.90);
        border-radius: 24px;
        box-shadow: 0 20px 60px rgba(29,45,58,0.22), inset 0 1px 0 #FFFFFF;
      }
      .modal-machine-header {
        margin: -15px -15px 14px -15px;
        padding: 14px 18px;
        background: linear-gradient(120deg, #234A66, #42647D);
        border-radius: 23px 23px 0 0;
        border-bottom: 3px solid var(--san-yellow);
      }
      .modal-machine-title {
        color: #FFFFFF;
        font-weight: 600;
      }
      .modal-close-x,
      .modal-close-x:hover,
      .modal-close-x:focus {
        color: #FFFFFF;
      }

      .modal-filters {
        position: relative;
        z-index: 30;
        overflow: visible;
        background: linear-gradient(135deg, rgba(255,255,255,0.86), rgba(231,239,245,0.72));
        border: 1px solid rgba(255,255,255,0.95);
        border-radius: 18px;
        box-shadow: inset 0 1px 0 #FFFFFF;
      }
      .modal-nav-bar {
        border-bottom-color: #D8DDE1;
      }
      .modal-nav-bar .btn-group .btn {
        color: var(--san-blue) !important;
        background: #FFFFFF !important;
        border-color: #CDD3D8 !important;
        font-weight: 600;
      }
      .modal-nav-bar .btn-group .btn:hover,
      .modal-nav-bar .btn-group .btn:focus {
        color: var(--san-blue) !important;
        background: #F2F4F5 !important;
        border-color: var(--san-yellow) !important;
      }
      .modal-nav-bar .btn-group .btn.active,
      .modal-nav-bar .btn-group .btn.active:hover,
      .modal-nav-bar .btn-group .btn.active:focus {
        color: #202326 !important;
        background: var(--san-yellow) !important;
        border-color: var(--san-yellow-dark) !important;
        box-shadow: none !important;
      }

      .titolo-sezione,
      .life-header-title,
      .life-card-title,
      .life-metric,
      .modal-machine-title,
      .documenti-title,
      .modal-documenti-title {
        color: var(--san-blue);
      }
      .modal-machine-title {
        color: #FFFFFF;
      }

      /* Card, report e tabelle: bianchi con bordi grigi puliti. */
      .life-card,
      .modal-data-panel,
      .report-table-wrap,
      .report-panel,
      .documenti-bar {
        background: linear-gradient(135deg, rgba(255,255,255,0.92), rgba(246,249,252,0.78));
        border-color: rgba(199,211,221,0.70);
        border-radius: 18px;
        box-shadow: 0 6px 20px rgba(35,74,102,0.06), inset 0 1px 0 #FFFFFF;
      }

      #nok_table th,
      #modal_nok_table th,
      .report-table th {
        background: var(--san-blue);
        color: #FFFFFF;
        border-bottom-color: var(--san-yellow);
      }
      #nok_table td,
      #modal_nok_table td,
      .report-table td {
        color: var(--san-text);
        border-bottom-color: #E3E6E8;
      }
      .report-table tbody tr:nth-child(even) {
        background: #F6F7F8;
      }

      .documento-download,
      .modal-data-button .btn {
        background: #FFFFFF;
        color: var(--san-blue) !important;
        border-color: #C7CED3;
      }
      .documento-download:hover,
      .documento-download:focus,
      .modal-data-button .btn:hover,
      .modal-data-button .btn:focus {
        background: #F7F8F8;
        color: var(--san-blue) !important;
        border-color: var(--san-yellow);
      }

      .report-summary {
        background: #F7F8F8;
        border-color: #D8DDE0;
        border-left-color: var(--san-yellow);
      }
      .report-section-number {
        background: var(--san-blue);
      }

      /* Il giallo identifica selezioni e interazioni. */
      .report-period-btn:hover,
      .report-period-btn:focus {
        border-color: var(--san-yellow) !important;
      }
      .report-period-btn.active {
        background: var(--san-yellow) !important;
        border-color: var(--san-yellow-dark) !important;
        color: #202326 !important;
      }

      .pannello-filtri .form-control,
      .modal-filters .form-control,
      .bootstrap-select > .dropdown-toggle,
      .documento-download,
      .modal-data-button .btn {
        border-radius: 12px;
      }
      .selectize-input {
        border-radius: 12px;
      }
      .report-summary {
        border-radius: 16px;
      }
      .modal-backdrop.in {
        opacity: 0.32;
      }
      @media (prefers-reduced-motion: reduce) {
        .home-corner .card-home {
          transition: none;
        }
      }

      /* I menu devono restare sopra schema, card e grafici. I modali Bootstrap
         conservano i propri livelli globali (backdrop 1040, modal 1050). */
      .pannello-filtri .selectize-dropdown,
      .modal-filters .selectize-dropdown,
      .pannello-filtri .dropdown-menu,
      .modal-filters .dropdown-menu {
        z-index: 1000;
      }
      .datepicker-dropdown {
        z-index: 1060 !important;
      }
      .selectize-dropdown,
      .bootstrap-select .dropdown-menu,
      .datepicker-dropdown {
        background: rgba(255,255,255,0.98);
        border: 1px solid rgba(197,205,211,0.70);
        border-radius: 14px;
        box-shadow: 0 14px 36px rgba(35,74,102,0.18), inset 0 1px 0 #FFFFFF;
      }
      .selectize-dropdown .active,
      .bootstrap-select .dropdown-menu > li > a:hover,
      .bootstrap-select .dropdown-menu > li > a:focus {
        background: #F0F2F3;
        color: var(--san-blue);
      }
      .home-corner .card-home:hover p,
      .home-corner .card-home:focus-visible p {
        color: var(--san-blue);
      }
      .home-corner .card-home .card-icona {
        color: var(--san-blue);
      }
      .modal-nav-bar .btn-group {
        padding: 4px;
        background: #F0F2F3;
        border: 1px solid #DDE1E4;
        border-radius: 16px;
      }
      .modal-nav-bar .btn-group .btn {
        border-radius: 12px !important;
        border-color: transparent !important;
        background: transparent !important;
        transition: background-color 0.18s ease, box-shadow 0.18s ease;
      }
      .modal-nav-bar .btn-group .btn.active {
        box-shadow: 0 2px 6px rgba(35,74,102,0.12) !important;
      }
      @media (max-width: 700px) {
        .home-corner .card-home .home-card-label {
          border: 0;
          border-radius: 0;
        }
      }

      /* Sistema visivo condiviso: controlli in vetro, contenuti su pannelli
         leggibili. Nessun blur sui grafici e nessun clipping sui filtri. */
      :root {
        --san-radius-lg: 24px;
        --san-radius-md: 16px;
        --san-shadow: 0 8px 28px rgba(35,74,102,0.07);
        --san-glass: linear-gradient(135deg, rgba(255,255,255,0.94), rgba(244,247,250,0.82));
      }
      body { -webkit-font-smoothing: antialiased; }
      .report-modal { font-family: Arial, 'Helvetica Neue', Helvetica, sans-serif; }
      .container-fluid > h2:first-of-type {
        background: linear-gradient(115deg, #234A66 10%, #3B5D77 72%, #5F748C);
        padding-top: 17px;
        padding-bottom: 17px;
        font-weight: 700;
        letter-spacing: -0.035em;
        box-shadow: 0 8px 28px rgba(35,74,102,0.12);
      }
      .pannello-filtri {
        padding-top: 14px;
        padding-bottom: 5px;
        background: var(--san-glass);
      }
      .pannello-filtri label, .modal-filters label {
        font-size: 12px;
        font-weight: 600;
        letter-spacing: 0.015em;
      }
      .form-control, .selectize-input,
      .bootstrap-select > .dropdown-toggle {
        border: 1px solid #DDE1E4;
        border-radius: 12px;
        background: #FFFFFF;
        color: var(--san-blue);
        box-shadow: 0 2px 5px rgba(35,74,102,0.025);
      }
      .selectize-input.focus, .form-control:focus,
      .bootstrap-select.open > .dropdown-toggle {
        border-color: var(--san-blue-2);
        box-shadow: 0 0 0 3px rgba(95,116,140,0.12);
      }
      .input-daterange .input-group-addon {
        background: transparent;
        border: 0;
        color: var(--san-muted);
      }
      .input-daterange input:first-child { border-radius: 12px 0 0 12px; }
      .input-daterange input:last-child { border-radius: 0 12px 12px 0; }
      .selectize-dropdown, .bootstrap-select .dropdown-menu, .datepicker-dropdown {
        padding: 6px;
        border-radius: 18px;
        box-shadow: 0 18px 44px rgba(35,74,102,0.18);
      }
      .selectize-dropdown .option,
      .bootstrap-select .dropdown-menu > li > a {
        padding: 9px 12px;
        border-radius: 9px;
      }
      .bootstrap-select .dropdown-menu > li.selected > a {
        background: rgba(235,189,85,0.20);
        color: var(--san-blue);
      }
      .datepicker table tr td, .datepicker table tr th {
        border-radius: 9px;
        width: 32px;
        height: 32px;
      }
      .datepicker table tr td.active,
      .datepicker table tr td.active:hover,
      .datepicker table tr td.selected,
      .datepicker table tr td.selected:hover {
        background: var(--san-yellow) !important;
        color: var(--san-blue) !important;
        text-shadow: none;
      }
      .datepicker table tr td.range {
        background: rgba(235,189,85,0.18);
      }
      .home-stage { border-radius: 28px; }
      .home-corner .card-home {
        border-radius: 26px;
        background-color: #F0F2F3;
        box-shadow: inset 0 0 0 1px rgba(255,255,255,0.95), inset 0 -1px 0 rgba(197,205,211,0.6);
      }
      .home-corner .card-home:hover,
      .home-corner .card-home:focus-visible { background-color: var(--san-yellow); }
      .home-corner .card-home .home-card-label {
        padding: 18px;
        width: clamp(180px, 14vw, 240px);
      }
      .home-corner .card-home h4 {
        font-size: 20px;
        letter-spacing: -0.025em;
        margin-bottom: 8px;
      }
      .home-corner .card-home p { font-size: 13px; line-height: 1.45; }
      .home-corner .card-home .card-icona {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 38px;
        height: 38px;
        margin-bottom: 10px;
        border: 1px solid rgba(255,255,255,0.95);
        border-radius: 13px;
        background: rgba(255,255,255,0.70);
        box-shadow: 0 3px 8px rgba(35,74,102,0.05);
        font-size: 18px;
      }
      .modal-content { border-radius: 28px; background: #F4F5F6; }
      .modal-machine-header {
        border-radius: 27px 27px 0 0;
        padding: 19px 22px;
        margin-bottom: 20px;
      }
      .modal-machine-title { font-size: 20px; letter-spacing: -0.02em; }
      .modal-close-x {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 36px;
        height: 36px;
        font-size: 26px;
        border-radius: 50%;
        background: rgba(255,255,255,0.12);
        opacity: 1;
      }
      .modal-close-x:hover { background: rgba(255,255,255,0.24); }
      .modal-nav-bar { border: 0; padding-bottom: 0; }
      .modal-nav-bar .btn-group {
        background: var(--san-glass);
        border-color: rgba(255,255,255,0.95);
        box-shadow: var(--san-shadow);
        border-radius: 20px;
        padding: 5px;
      }
      .modal-nav-bar .btn-group .btn {
        padding: 11px 20px;
        border-radius: 15px !important;
      }
      .modal-filters {
        padding: 18px 20px 8px;
        border-radius: 22px;
        background: var(--san-glass);
        box-shadow: var(--san-shadow), inset 0 1px 0 #FFFFFF;
      }
      .shiny-input-radiogroup .shiny-options-group {
        display: flex;
        flex-wrap: wrap;
        gap: 5px;
      }
      .shiny-input-radiogroup label.radio-inline {
        position: relative;
        margin: 0;
        padding: 7px 12px;
        border: 1px solid #DDE1E4;
        border-radius: 999px;
        background: rgba(255,255,255,0.85);
        color: var(--san-muted);
        cursor: pointer;
      }
      .shiny-input-radiogroup label.radio-inline:has(input:checked) {
        background: var(--san-yellow);
        border-color: var(--san-yellow);
        color: var(--san-blue);
      }
      .shiny-input-radiogroup label.radio-inline input {
        position: static;
        margin: 0 5px 0 0;
        vertical-align: middle;
      }
      .shiny-input-radiogroup label.radio-inline:focus-within {
        outline: 2px solid var(--san-blue);
        outline-offset: 2px;
      }
      .titolo-sezione, .life-header-title {
        font-size: 23px;
        font-weight: 700;
        letter-spacing: -0.035em;
        margin-top: 22px;
        margin-bottom: 15px;
      }
      .activation-scroll, .girafe.html-widget {
        border: 1px solid #E6EBEF;
        border-radius: 22px;
        background: #FFFFFF;
        box-shadow: var(--san-shadow);
      }
      .activation-scroll .girafe.html-widget {
        border: 0;
        border-radius: 0;
        box-shadow: none;
      }
      .life-card, .report-kpi-card, .report-panel, .modal-data-panel,
      .report-table-wrap, .documenti-bar {
        border-radius: 22px;
        border-color: rgba(197,205,211,0.55);
        background: #FFFFFF;
        box-shadow: var(--san-shadow);
      }
      .life-card { padding: 20px; }
      .life-card-title { font-size: 16px; letter-spacing: -0.02em; }
      .life-card-header { border-bottom-color: #EDF0F2; padding-bottom: 15px; }
      .life-value { font-variant-numeric: tabular-nums; font-size: 13px; }
      .life-progress { height: 10px; background: #EDF0F2; }
      .life-status { padding: 6px 11px; }
      .report-kpi-card { padding: 20px; }
      .report-kpi-value { font-size: 30px; letter-spacing: -0.035em; }
      .report-kpi-label { letter-spacing: 0.04em; }
      .report-summary { border-radius: 22px; background: #FFFFFF; box-shadow: var(--san-shadow); }
      .report-hero { border-radius: 27px 27px 22px 22px; box-shadow: var(--san-shadow); }
      .report-download.btn { border-radius: 999px !important; }
      .modal-content .btn-default, .documento-download, .report-period-btn {
        border-radius: 999px;
        border: 1px solid #DDE1E4;
        background: #FFFFFF;
        color: var(--san-blue);
        padding: 8px 15px;
        box-shadow: 0 3px 8px rgba(35,74,102,0.04);
        font-weight: 600;
      }
      .modal-content .btn-default:hover, .documento-download:hover {
        background: #F0F2F3;
        border-color: #C5CDD3;
      }
      .modal-content table { font-variant-numeric: tabular-nums; }
      #nok_table th, #modal_nok_table th, .report-table th,
      .modal-content table.dataTable thead th {
        background: #F0F2F3;
        color: var(--san-blue);
        border-bottom: 1px solid #DDE1E4;
        padding: 12px;
        font-size: 12px;
        font-weight: 700;
      }
      #nok_table td, #modal_nok_table td, .report-table td,
      .modal-content table.dataTable tbody td {
        padding: 12px;
        border-bottom: 1px solid #EDF0F2;
      }
      .report-table tbody tr:nth-child(even),
      .modal-content table.dataTable.stripe tbody tr.odd { background: #FAFBFC; }
      .modal-content table.dataTable tbody tr:hover { background: #F0F2F3; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: var(--san-yellow) !important;
        border: 0 !important;
        border-radius: 10px;
        color: var(--san-blue) !important;
      }
      .alarm-empty { border-radius: 18px; background: #FFFFFF; padding: 18px; }
      .sensor-tooltip { border-radius: 14px; box-shadow: 0 12px 32px rgba(35,74,102,0.20); }
      @media (max-width: 1050px) {
        .home-corner .card-home .home-card-label { width: 150px; padding: 12px; }
        .home-corner .card-home h4 { font-size: 17px; }
      }
      @media (max-width: 700px) {
        .home-corner .card-home .home-card-label { width: 100%; padding: 8px; }
        .modal-nav-bar .btn-group .btn { padding: 9px 10px; font-size: 12px; }
        .modal-filters { padding: 14px 12px 4px; }
      }
      @media (prefers-reduced-motion: reduce) {
        .modal-nav-bar .btn-group .btn { transition: none; }
      }

      /* Dettagli di interazione: tempi brevi, bordi coerenti e focus leggibile. */
      :root {
        --san-ease: cubic-bezier(0.22, 1, 0.36, 1);
        --san-fast: 150ms;
        --san-motion: 180ms;
      }
      .btn, .documento-download, .modal-close-x, .sensor-hotspot,
      .selectize-input, .form-control, .radio-inline {
        -webkit-tap-highlight-color: transparent;
        touch-action: manipulation;
      }
      .btn, .documento-download, .modal-close-x, .sensor-hotspot,
      .selectize-dropdown .option, .datepicker td.day, .datepicker th.prev,
      .datepicker th.next, .datepicker th.datepicker-switch {
        cursor: pointer;
      }
      input.form-control, .selectize-input input { cursor: text; }
      button:disabled, .btn.disabled, .btn[disabled],
      .datepicker td.disabled, .datepicker td.disabled:hover {
        cursor: not-allowed;
      }
      .btn.disabled, .btn[disabled], button:disabled {
        opacity: 0.48;
        box-shadow: none !important;
        transform: none !important;
      }
      .btn:not(.card-home), .documento-download, .modal-close-x {
        transition: background-color var(--san-fast) ease,
          border-color var(--san-fast) ease,
          box-shadow var(--san-fast) ease,
          transform var(--san-fast) var(--san-ease);
      }
      @media (hover: hover) and (pointer: fine) {
        .modal-data-button .btn:hover,
        .documento-download:hover, .report-download.btn:hover {
          transform: translateY(-1px);
          box-shadow: 0 5px 14px rgba(35,74,102,0.10);
        }
      }
      .btn:not(.card-home):not(.dropdown-toggle):active,
      .documento-download:active, .modal-close-x:active {
        transform: scale(0.975);
      }
      .home-corner .card-home:active {
        background-color: var(--san-yellow-dark);
        box-shadow: inset 0 2px 10px rgba(35,74,102,0.12);
      }
      .btn:focus-visible, .documento-download:focus-visible,
      .modal-close-x:focus-visible, .sensor-hotspot:focus-visible {
        outline: 2px solid var(--san-blue) !important;
        outline-offset: 3px;
      }
      .home-corner .card-home:focus-visible { outline-offset: -6px; }
      .modal-close-x:focus-visible, .report-back-btn:focus-visible,
      .report-download:focus-visible { outline-color: #FFFFFF !important; }
      .modal-content .btn-default, .documento-download {
        background-image: linear-gradient(180deg, rgba(255,255,255,0.55), rgba(244,245,246,0.12));
        box-shadow: inset 0 1px 0 #FFFFFF, 0 2px 6px rgba(35,74,102,0.04);
        line-height: 1.4;
      }
      .modal-nav-bar .btn-group .btn {
        line-height: 1.4;
        letter-spacing: -0.01em;
      }
      .modal-nav-bar .btn-group .btn.active {
        box-shadow: inset 0 1px 0 rgba(255,255,255,0.65), 0 3px 8px rgba(35,74,102,0.10) !important;
      }
      .modal-content, .life-card, .report-kpi-card, .report-panel,
      .modal-data-panel, .documenti-bar {
        border-color: rgba(197,205,211,0.55);
        box-shadow: inset 0 1px 0 rgba(255,255,255,0.95), var(--san-shadow);
      }
      .modal-content {
        box-shadow: inset 0 1px 0 #FFFFFF, 0 24px 70px rgba(35,74,102,0.22);
      }
      /* Usa il ciclo nativo Bootstrap anche in chiusura: nessun timer
         che possa ritardare o bloccare i messaggi Shiny. */
      #shiny-modal.fade {
        transition: opacity var(--san-motion) ease;
      }
      #shiny-modal.fade .modal-dialog {
        transform: translateY(12px) scale(0.985);
        transition: transform var(--san-motion) var(--san-ease);
      }
      #shiny-modal.in .modal-dialog, #shiny-modal.show .modal-dialog {
        transform: translateY(0) scale(1);
      }
      .modal-backdrop.fade { transition: opacity var(--san-motion) ease; }
      @keyframes san-panel-reveal {
        from { opacity: 0; }
        to { opacity: 1; }
      }
      .modal-data-panel, .selectize-dropdown,
      .bootstrap-select.open > .dropdown-menu, .datepicker-dropdown {
        animation: san-panel-reveal 160ms ease-out;
      }
      .sensor-hotspot { transition: background-color 140ms ease, border-color 140ms ease; }
      .sensor-hotspot .sensor-tooltip {
        display: block;
        opacity: 0;
        visibility: hidden;
        transition: opacity 140ms ease, visibility 140ms ease;
        padding: 12px 15px;
        line-height: 1.5;
        white-space: normal;
        width: max-content;
        max-width: min(300px, calc(100vw - 36px));
        min-width: min(220px, calc(100vw - 36px));
      }
      .sensor-hotspot:hover .sensor-tooltip,
      .sensor-hotspot:focus .sensor-tooltip {
        opacity: 1;
        visibility: visible;
        transition-delay: 90ms;
      }
      .sensor-hotspot:focus-visible .sensor-tooltip { transition-delay: 0ms; }
      .modal-machine-title, .report-title, .life-card-title {
        overflow-wrap: anywhere;
        text-wrap: balance;
      }
      .home-card-label p, .report-subtitle, .life-header-subtitle {
        text-wrap: pretty;
      }
      .report-kpi-value, .life-value, .report-table td {
        font-variant-numeric: tabular-nums;
      }
      .modal-data-button .btn { min-width: 125px; }
      .shiny-input-radiogroup label.radio-inline input {
        position: absolute;
        width: 1px;
        height: 1px;
        padding: 0;
        margin: -1px;
        overflow: hidden;
        clip: rect(0, 0, 0, 0);
        white-space: nowrap;
      }
      .shiny-input-radiogroup label.radio-inline:hover {
        border-color: #C5CDD3;
        color: var(--san-blue);
      }
      .shiny-input-radiogroup label.radio-inline:has(input:checked) {
        box-shadow: inset 0 1px 0 rgba(255,255,255,0.65), 0 2px 5px rgba(35,74,102,0.06);
      }
      .activation-scroll, .modal-body, .selectize-dropdown-content,
      .bootstrap-select .dropdown-menu.inner, .dataTables_scrollBody {
        scrollbar-width: thin;
        scrollbar-color: #AAB8C3 transparent;
      }
      .activation-scroll::-webkit-scrollbar,
      .modal-body::-webkit-scrollbar { width: 9px; height: 9px; }
      .activation-scroll::-webkit-scrollbar-thumb,
      .modal-body::-webkit-scrollbar-thumb {
        background: #AAB8C3;
        border: 2px solid #F4F5F6;
        border-radius: 999px;
      }
      .activation-scroll::-webkit-scrollbar-thumb:hover,
      .modal-body::-webkit-scrollbar-thumb:hover { background: var(--san-blue-2); }
      .recalculating { cursor: progress; }
      ::selection { background: rgba(235,189,85,0.35); color: #234A66; }
      @media (prefers-reduced-motion: reduce) {
        #shiny-modal.fade, #shiny-modal.fade .modal-dialog, .modal-backdrop.fade,
        .btn, .documento-download, .modal-close-x, .sensor-hotspot,
        .sensor-hotspot .sensor-tooltip, .home-corner .card-home,
        .home-corner .card-home .home-card-label {
          transition: none !important;
          transition-delay: 0ms !important;
        }
        #shiny-modal.fade .modal-dialog { transform: none; }
        .modal-data-panel, .selectize-dropdown,
        .bootstrap-select.open > .dropdown-menu, .datepicker-dropdown {
          animation: none;
        }
        .modal-data-button .btn:hover, .documento-download:hover,
        .report-download.btn:hover, .btn:active, .modal-close-x:active {
          transform: none !important;
        }
      }
    "))
}
