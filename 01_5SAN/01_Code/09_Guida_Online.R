# Guida per chi consulta la macchina dal sito: contenuto statico, senza dati
# di altre aziende o dettagli di implementazione. Caricata da 05_Plots.R.
guida_online_assets <- function() {
  tagList(
    tags$style(HTML(r"---(
      .san-guide-action { display: flex; justify-content: flex-end; padding-top: 22px; }
      #home_guida.san-guide-button {
        display: inline-flex; align-items: center; gap: 7px;
        padding: 8px 15px; margin: 0; min-height: 34px;
        background: #FFFFFF; color: #234A66; border: 1px solid #BCC9D2;
        border-radius: 12px; font-size: 13px; font-weight: 600; line-height: 1.3;
        box-shadow: none;
      }
      #home_guida.san-guide-button:hover { background: #EBBD55; border-color: #EBBD55; }
      #home_guida.san-guide-button:focus-visible,
      .san-guide a:focus-visible, .san-guide-close:focus-visible {
        outline: 3px solid #234A66; outline-offset: 3px;
      }
      #shiny-modal:has(.san-guide) .modal-dialog {
        width: calc(100% - 32px); max-width: 1120px; margin: 24px auto;
      }
      #shiny-modal:has(.san-guide) .modal-content {
        border-radius: 22px; overflow: hidden; background: #FFFFFF;
      }
      #shiny-modal:has(.san-guide) .modal-body {
        padding: 0; max-height: none; overflow: hidden;
      }
      .san-guide {
        height: 78vh; height: min(780px, 82dvh);
        max-height: calc(100dvh - 48px); display: flex; flex-direction: column;
        color: #263C4C; background: #FFFFFF; font-size: 15px; line-height: 1.65;
      }
      .san-guide * { box-sizing: border-box; }
      .san-guide-header {
        display: flex; align-items: flex-start; justify-content: space-between;
        gap: 20px; padding: 22px 28px 19px; background: #234A66;
        color: #FFFFFF; border-bottom: 2px solid #EBBD55; flex-shrink: 0;
      }
      .san-guide h2.san-guide-title {
        font-size: 23px; font-weight: 700; color: #FFFFFF;
        line-height: 1.25; margin: 0 0 6px; letter-spacing: -0.02em;
      }
      .san-guide-header p { margin: 0; color: #E0E8EE; font-size: 14px; }
      .san-guide-close {
        flex-shrink: 0; display: inline-flex; align-items: center; justify-content: center;
        width: 36px; height: 36px; border: 1px solid #6E879A;
        border-radius: 50%; background: transparent; color: #FFFFFF;
        font-size: 26px; line-height: 1; cursor: pointer;
      }
      .san-guide-close:hover { background: #3B5D77; }
      .san-guide-close:focus-visible { outline-color: #EBBD55; }
      .san-guide-layout {
        display: grid; grid-template-columns: 172px minmax(0, 1fr);
        min-height: 0; flex: 1;
      }
      .san-guide-nav {
        min-height: 0; overflow-y: auto; padding: 22px 12px;
        background: #F4F6F8; border-right: 1px solid #E0E6EB;
      }
      .san-guide-nav-label {
        display: block; padding: 0 11px 10px; color: #526676;
        font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.08em;
      }
      .san-guide-nav a {
        display: block; padding: 9px 10px; margin: 0 0 3px;
        border-left: 3px solid transparent; border-radius: 7px;
        color: #425A6C; font-size: 12px; line-height: 1.4; text-decoration: none;
      }
      .san-guide-nav a:hover { background: #E5ECF1; color: #234A66; }
      .san-guide-nav a[aria-current="location"] {
        background: #E4ECF2; border-left-color: #C38C18; color: #234A66; font-weight: 700;
      }
      .san-guide-content {
        min-width: 0; min-height: 0; overflow-y: auto; overscroll-behavior: contain;
        padding: 26px 34px 38px; scrollbar-gutter: stable;
      }
      .san-guide-section { margin: 0 0 30px; padding: 0 0 28px; border-bottom: 1px solid #E4E9ED; }
      .san-guide-section:last-child { margin-bottom: 0; padding-bottom: 0; border-bottom: 0; }
      .san-guide h3 {
        margin: 0 0 14px; color: #234A66; font-size: 21px; font-weight: 700; line-height: 1.3;
      }
      .san-guide h4 { margin: 18px 0 7px; color: #234A66; font-size: 15px; font-weight: 700; }
      .san-guide p { margin: 0 0 12px; }
      .san-guide ul, .san-guide ol { padding-left: 22px; margin: 0 0 14px; }
      .san-guide li { margin-bottom: 7px; padding-left: 2px; }
      .san-guide strong { font-weight: 700; color: #234A66; }
      .san-guide table { width: 100%; border-collapse: collapse; margin: 14px 0; font-size: 14px; line-height: 1.55; }
      .san-guide th, .san-guide td { padding: 11px 12px; border: 1px solid #DDE5EB; text-align: left; vertical-align: top; overflow-wrap: break-word; }
      .san-guide th { background: #F0F4F7; color: #234A66; font-weight: 700; }
      .san-guide td:first-child { width: 29%; }
      .san-guide-note { color: #526676; font-size: 13px; }
      .san-guide-example { padding-left: 14px; border-left: 2px solid #D7B46D; }
      .san-guide-section h3:focus { outline: none; }
      .san-guide-section h3:focus-visible { outline: 2px solid #234A66; outline-offset: 4px; }
      @media (max-width: 700px) {
        .san-guide-action { padding-top: 0; padding-bottom: 10px; }
        #shiny-modal:has(.san-guide) .modal-dialog { width: calc(100% - 16px); margin: 8px auto; }
        .san-guide { height: calc(100dvh - 16px); max-height: calc(100dvh - 16px); }
        .san-guide-header { padding: 18px; gap: 12px; }
        .san-guide h2.san-guide-title { font-size: 20px; }
        .san-guide-header p { font-size: 13px; }
        .san-guide-layout { display: flex; flex-direction: column; }
        .san-guide-nav {
          display: flex; gap: 4px; padding: 9px 12px; flex-shrink: 0;
          overflow-x: auto; overflow-y: hidden; border-right: 0; border-bottom: 1px solid #E0E6EB;
        }
        .san-guide-nav-label { display: none; }
        .san-guide-nav a { white-space: nowrap; margin: 0; padding: 9px 10px; border-left: 0; border-bottom: 3px solid transparent; }
        .san-guide-nav a[aria-current="location"] { border-left: 0; border-bottom-color: #C38C18; }
        .san-guide-content { flex: 1; padding: 22px 18px 30px; }
        .san-guide h3 { font-size: 20px; }
        .san-guide th, .san-guide td { padding: 9px; }
        .san-guide table { font-size: 13px; }
      }
      @media (prefers-reduced-motion: reduce) {
        .san-guide-content { scroll-behavior: auto; }
      }
    )---")),
    tags$script(HTML(r"---(
      (function () {
        if (window.sanGuideBound) return;
        window.sanGuideBound = true;

        function updateGuideIndex(guide) {
          var content = guide.querySelector('.san-guide-content');
          var sections = guide.querySelectorAll('.san-guide-section');
          var links = guide.querySelectorAll('.san-guide-nav a');
          if (!content || !sections.length) return;
          var active = sections[0].id;
          var top = content.getBoundingClientRect().top + 42;
          sections.forEach(function (section) {
            if (section.getBoundingClientRect().top <= top) active = section.id;
          });
          if (content.scrollTop + content.clientHeight >= content.scrollHeight - 3) {
            active = sections[sections.length - 1].id;
          }
          links.forEach(function (link) {
            if (link.getAttribute('href') === '#' + active) {
              link.setAttribute('aria-current', 'location');
            } else {
              link.removeAttribute('aria-current');
            }
          });
        }

        $(document).on('shown.bs.modal.sanGuide', '#shiny-modal', function () {
          var guide = this.querySelector('.san-guide');
          if (!guide) return;
          var content = guide.querySelector('.san-guide-content');
          if (!content.sanGuideScrollBound) {
            content.sanGuideScrollBound = true;
            var pending = false;
            content.addEventListener('scroll', function () {
              if (pending) return;
              pending = true;
              window.requestAnimationFrame(function () {
                pending = false;
                updateGuideIndex(guide);
              });
            }, {passive: true});
          }
          updateGuideIndex(guide);
          guide.querySelector('.san-guide-title').focus({preventScroll: true});
        });

        $(document).on('click.sanGuide', '.san-guide-nav a', function (event) {
          var guide = this.closest('.san-guide');
          var id = this.getAttribute('href').slice(1);
          var section = guide.querySelector('#' + id);
          if (!section) return;
          event.preventDefault();
          var content = guide.querySelector('.san-guide-content');
          var offset = section.getBoundingClientRect().top - content.getBoundingClientRect().top;
          content.scrollTo({
            top: content.scrollTop + offset - 24,
            behavior: window.matchMedia('(prefers-reduced-motion: reduce)').matches ? 'auto' : 'smooth'
          });
          section.querySelector('h3').focus({preventScroll: true});
        });
      }());
    )---"))
  )
}

guida_online_modal <- function() {
  modalDialog(
    title = NULL, size = "xl", easyClose = TRUE, fade = TRUE, footer = NULL,
    div(
      class = "san-guide",
      div(
        class = "san-guide-header",
        div(
          h2("Guida alla dashboard", class = "san-guide-title", tabindex = "-1"),
          p("Come consultare i dati della macchina e interpretare le informazioni mostrate")
        ),
        tags$button(
          type = "button", class = "san-guide-close", `data-dismiss` = "modal",
          `aria-label` = "Chiudi la guida", title = "Chiudi la guida (Esc)", HTML("&times;")
        )
      ),
      div(
        class = "san-guide-layout",
        tags$nav(
          class = "san-guide-nav", `aria-label` = "Indice della guida",
          span("In questa guida", class = "san-guide-nav-label"),
          tags$a(href = "#guida-inizio", "Primi passi", `aria-current` = "location"),
          tags$a(href = "#guida-manovre", "Conteggio manovre"),
          tags$a(href = "#guida-utilizzo", "Utilizzo storico"),
          tags$a(href = "#guida-disponibilita", "Disponibilità"),
          tags$a(href = "#guida-vita", "Vita componenti"),
          tags$a(href = "#guida-segnalazioni", "Segnalazioni"),
          tags$a(href = "#guida-report", "Report e documenti"),
          tags$a(href = "#guida-dubbi", "Dubbi frequenti")
        ),
        tags$article(
          class = "san-guide-content", tabindex = "0", `aria-label` = "Contenuto della guida",
          HTML(r"---(
<section id="guida-inizio" class="san-guide-section" aria-labelledby="guida-inizio-titolo">
  <h3 id="guida-inizio-titolo" tabindex="-1">Primi passi</h3>
  <p>La dashboard mostra i dati dei <strong>componenti di sicurezza monitorati sulla macchina</strong>: le manovre registrate, l’utilizzo rispetto al passato, la disponibilità e le situazioni da approfondire.</p>
  <ol>
    <li><strong>Scegli il periodo</strong> nella parte alta della pagina.</li>
    <li><strong>Apri una delle quattro viste</strong> per consultare i dettagli. All’interno puoi selezionare uno o più componenti.</li>
    <li><strong>Passa il puntatore sui grafici</strong> per leggere i valori. Usa <strong>Mostra dati</strong>, dove presente, per aprire la tabella.</li>
  </ol>
  <p>La macchina è già associata alla pagina aperta dal sito. Quando disponibile, lo schema centrale permette di individuare i componenti: passa il puntatore su un punto per leggerne le informazioni o fai clic per aprire il relativo conteggio.</p>
  <p class="san-guide-note">Per tornare ai dati chiudi questa guida con la × o con Esc. L’indice permette di raggiungere direttamente ogni argomento.</p>
</section>
<section id="guida-manovre" class="san-guide-section" aria-labelledby="guida-manovre-titolo">
  <h3 id="guida-manovre-titolo" tabindex="-1">Conteggio manovre</h3>
  <p>Una <strong>manovra</strong> è un’attivazione registrata dal componente. Il grafico a barre confronta i componenti; <strong>Andamento per componente</strong> mostra come cambiano nel tempo.</p>
  <table>
    <thead><tr><th scope="col">Raggruppamento</th><th scope="col">Come leggere il valore</th></tr></thead>
    <tbody>
      <tr><td>Ora</td><td>Manovre registrate nella singola ora.</td></tr>
      <tr><td>Giorno</td><td>Manovre registrate nella giornata.</td></tr>
      <tr><td>Settimana, mese e intervalli maggiori</td><td>Media giornaliera delle manovre nei giorni con dati dell’intervallo.</td></tr>
    </tbody>
  </table>
  <p class="san-guide-example"><strong>Esempio.</strong> Tre giorni con 8, 10 e 12 manovre danno una media di <strong>10 manovre al giorno</strong>. Nel raggruppamento settimanale il grafico mostra 10, non il totale di 30.</p>
  <p>Il dettaglio al passaggio del puntatore riporta anche le ore macchina: per intervalli maggiori del giorno si tratta di ore medie giornaliere. Se noti un picco, passa a Giorno o Ora e seleziona il componente interessato.</p>
  <p class="san-guide-note">Una manovra non corrisponde necessariamente a un pezzo prodotto. I giorni senza dati non sono automaticamente giorni con zero manovre.</p>
</section>
<section id="guida-utilizzo" class="san-guide-section" aria-labelledby="guida-utilizzo-titolo">
  <h3 id="guida-utilizzo-titolo" tabindex="-1">Utilizzo storico</h3>
  <p>Questa vista confronta il comportamento dei componenti con il loro passato, tenendo conto delle ore di accensione osservate.</p>
  <h4>Il NOK in parole semplici</h4>
  <p>Il <strong>NOK</strong> indica quanto il ritmo medio di manovre per ora si discosta dal riferimento storico dello stesso componente.</p>
  <table>
    <thead><tr><th scope="col">Valore</th><th scope="col">Significato</th></tr></thead>
    <tbody>
      <tr><td>NOK = 1</td><td>Ritmo in linea con il riferimento storico.</td></tr>
      <tr><td>NOK = 2</td><td>Circa il doppio delle manovre per ora.</td></tr>
      <tr><td>NOK = 0,5</td><td>Circa la metà delle manovre per ora.</td></tr>
    </tbody>
  </table>
  <p>Il grafico nel tempo aiuta a capire <strong>quando è cambiato il ritmo</strong>. Un valore maggiore di 1 non indica automaticamente un problema: conta anche quanto il comportamento varia abitualmente.</p>
  <h4>Profilo e indice di utilizzo</h4>
  <p>Nel profilo, il punto mostra il livello medio del componente e la fascia attorno al punto mostra quanto variano i giorni. L’indice di utilizzo riassume <strong>intensità e variabilità</strong> dei componenti disponibili.</p>
  <p>Le tessere <strong>Basso, Normale ed Elevato</strong> confrontano l’indice con il comportamento storico della macchina. Puoi scegliere Settimana o Mese e aprire <strong>Utilizzo storico</strong> per seguirne l’evoluzione. Sono giudizi relativi al passato, non limiti di sicurezza.</p>
  <p class="san-guide-note">Il NOK e l’indice non sono percentuali di produzione o di vita residua. Controlla le date riportate: lo storico precedente nei grafici può coprire un intervallo più lungo del periodo scelto.</p>
</section>
<section id="guida-disponibilita" class="san-guide-section" aria-labelledby="guida-disponibilita-titolo">
  <h3 id="guida-disponibilita-titolo" tabindex="-1">Disponibilità della macchina</h3>
  <p>Nella vista Utilizzo storico, la disponibilità indica <strong>quanta parte del tempo di accensione osservato</strong> presenta tutti i componenti monitorati contemporaneamente nella condizione prevista per rendere la macchina disponibile al lavoro.</p>
  <p class="san-guide-example"><strong>Esempio.</strong> 6 ore disponibili su 8 ore di accensione corrispondono al <strong>75%</strong>. Il dettaglio del grafico riporta sia la percentuale sia le ore.</p>
  <ul>
    <li>Il riferimento è il tempo di accensione osservato, non tutte le ore del calendario.</li>
    <li>La percentuale riguarda <strong>l’intera macchina monitorata</strong>: togliere un componente dai filtri non lo esclude da questo calcolo.</li>
    <li>Una macchina disponibile può essere ferma: il valore non indica per quanto tempo ha effettivamente prodotto.</li>
  </ul>
  <p class="san-guide-note">Una disponibilità bassa richiede di verificare gli stati dei componenti e la completezza dei dati, anche attraverso le segnalazioni.</p>
</section>
<section id="guida-vita" class="san-guide-section" aria-labelledby="guida-vita-titolo">
  <h3 id="guida-vita-titolo" tabindex="-1">Vita componenti</h3>
  <p>Ogni scheda confronta il consumo accumulato con due riferimenti del componente: <strong>manovre effettuate</strong> rispetto a B10dSAN e <strong>anni di utilizzo</strong> rispetto a T10d.</p>
  <table>
    <thead><tr><th scope="col">Stato</th><th scope="col">Come leggerlo</th></tr></thead>
    <tbody>
      <tr><td>OK</td><td>Le percentuali disponibili sono inferiori all’80%.</td></tr>
      <tr><td>Attenzione</td><td>Almeno una percentuale è fra l’80% e il 100%, senza superamenti.</td></tr>
      <tr><td>Superato</td><td>Almeno uno dei due riferimenti è oltre il 100%.</td></tr>
    </tbody>
  </table>
  <p>La percentuale residua è la parte del riferimento non ancora raggiunta: <strong>non prevede il momento di un guasto</strong>. Controlla entrambe le righe; una può essere non disponibile anche se l’altra ha un valore.</p>
  <p>Questa vista mostra lo stato cumulato disponibile, non soltanto il consumo del periodo selezionato. Se manca una durata comunicata dal componente, gli anni sono stimati dalla prima osservazione disponibile: possono quindi non coincidere con l’età reale.</p>
  <p class="san-guide-note">Usa queste informazioni per pianificare un approfondimento con il referente della macchina, insieme alla documentazione del componente.</p>
</section>
<section id="guida-segnalazioni" class="san-guide-section" aria-labelledby="guida-segnalazioni-titolo">
  <h3 id="guida-segnalazioni-titolo" tabindex="-1">Allarmi e segnalazioni</h3>
  <p>Qui trovi eventi e comportamenti che meritano una verifica. Seleziona il periodo e i componenti, poi leggi la descrizione e il dettaglio di ogni riga.</p>
  <table>
    <thead><tr><th scope="col">Tipo</th><th scope="col">Cosa evidenzia</th></tr></thead>
    <tbody>
      <tr><td>Manovre anomale</td><td>Giornate con più o meno manovre del solito, tenendo conto delle ore osservate, oppure picchi nelle singole ore.</td></tr>
      <tr><td>Componenti aperti o senza contatto elettrico</td><td>Una condizione osservata di oltre un’ora consecutiva, con conteggio invariato.</td></tr>
      <tr><td>NOK</td><td>Un ritmo giornaliero per ora molto superiore o inferiore al riferimento del componente.</td></tr>
      <tr><td>Interruzioni delle comunicazioni</td><td>Possibili interruzioni dell’invio dei dati, seguite da nuove manovre registrate alla ripresa.</td></tr>
    </tbody>
  </table>
  <p><strong>Una riga può coinvolgere più componenti.</strong> I casi dello stesso tipo, nello stesso intervallo e con lo stesso comportamento possono essere riuniti. Il dettaglio permette di riconoscere i singoli componenti.</p>
  <p><strong>L’ID identifica la segnalazione.</strong> Conserva questo codice, il periodo e i componenti interessati quando chiedi una verifica. Le segnalazioni salvate restano nello storico; cambiare filtro non le ricalcola.</p>
  <p class="san-guide-note">Le segnalazioni dei componenti richiedono almeno 30 giornate con dati validi; per alcune analisi serve anche un riferimento sufficiente. Le giornate vengono consolidate dopo la loro conclusione: l’elenco può non includere il giorno in corso.</p>
</section>
<section id="guida-report" class="san-guide-section" aria-labelledby="guida-report-titolo">
  <h3 id="guida-report-titolo" tabindex="-1">Report e documenti</h3>
  <ol>
    <li>Apri <strong>Allarmi e segnalazioni</strong> e individua <strong>Report anomalie</strong>.</li>
    <li>Scegli <strong>Settimana</strong> o <strong>Mese</strong>, poi premi il pulsante dell’intervallo.</li>
    <li>Leggi la sintesi e i dettagli; usa <strong>Scarica PDF</strong> per conservare il rapporto.</li>
  </ol>
  <p>Il rapporto riepiloga manovre medie giornaliere, utilizzo rispetto allo storico e situazioni insolite. Le variazioni fra parentesi confrontano il periodo con quello immediatamente precedente della stessa durata.</p>
  <p><strong>Controlla le date.</strong> Nei report le fasce settimanali sono 1–7, 8–14, 15–21 e 22–fine mese. L’ultima può durare più di sette giorni. Se il filtro taglia una fascia, il rapporto considera il tratto selezionato.</p>
  <p>Il rapporto descrive l’intero periodo con i dati disponibili; l’elenco delle segnalazioni conserva invece i singoli eventi consolidati. <strong>I risultati e il numero di casi possono quindi differire.</strong></p>
  <p class="san-guide-note">Quando presenti, i documenti scaricabili nella sezione Documenti accompagnano la verifica delle segnalazioni.</p>
</section>
<section id="guida-dubbi" class="san-guide-section" aria-labelledby="guida-dubbi-titolo">
  <h3 id="guida-dubbi-titolo" tabindex="-1">Dubbi frequenti</h3>
  <h4>Che cosa significa N/D?</h4>
  <p>Il valore non è disponibile o non è calcolabile: possono mancare misure, ore osservate, uno storico sufficiente o un riferimento del componente. <strong>N/D non equivale a zero.</strong></p>
  <h4>Perché non vedo dati o segnalazioni?</h4>
  <p>Controlla periodo e componenti selezionati. Un grafico vuoto può dipendere da dati mancanti; nessuna segnalazione può anche dipendere da uno storico ancora insufficiente. Se la pagina indica che non ci sono dati associati alla macchina, rivolgiti al referente del servizio.</p>
  <h4>Molte manovre significano un’anomalia?</h4>
  <p>Non necessariamente: possono essere abituali. Leggi anche NOK e vita accumulata. Dopo un’interruzione delle comunicazioni, alcune manovre possono comparire insieme alla ripresa: verifica il relativo avviso prima di attribuire il picco a quell’ora.</p>
  <h4>Una segnalazione conferma un guasto?</h4>
  <p>No: individua una situazione da approfondire nel contesto operativo. Allo stesso modo, l’assenza di segnalazioni non certifica la sicurezza della macchina. Per la verifica, coinvolgi il referente della tua azienda indicando macchina, periodo e ID della segnalazione.</p>
</section>
          )---")
        )
      )
    )
  )
}
