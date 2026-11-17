dashboard_script <- function() {
    tags$script(HTML("
      function aggiornaLarghezzaModal() {
        setTimeout(function () {
          var el = document.querySelector('.modal-body');

          if (el && el.clientWidth > 0) {
            Shiny.setInputValue(
              'modal_px_width',
              el.clientWidth,
              {priority: 'event'}
            );
          }
        }, 100);
      }

      function aggiornaLayoutHome() {
        var stage = document.querySelector('.home-stage');
        var frame = stage ? stage.querySelector('.schema-frame') : null;
        var img = frame ? frame.querySelector('img') : null;

        if (!stage || !frame || !img || window.innerWidth <= 700) {
          return;
        }

        if (img.naturalWidth > 0 && img.naturalHeight > 0) {
          var rapporto = img.naturalWidth / img.naturalHeight;
          var stageWidth = stage.clientWidth;
          var stageHeight = stage.clientHeight;

          var isLargeSiboni = frame.classList.contains('schema-frame--siboni-large');
          var maxWidth = Math.min(stageWidth * (isLargeSiboni ? 0.96 : 0.84), 1500);
          var maxHeight = stageHeight * (isLargeSiboni ? 0.98 : 0.86);
          var targetWidth = Math.min(maxWidth, maxHeight * rapporto);

          frame.style.width = Math.max(560, targetWidth) + 'px';
        }
      }

      function pianificaLayoutHome() {
        window.requestAnimationFrame(function () {
          setTimeout(aggiornaLayoutHome, 40);
        });
      }

      $(document).on('shown.bs.modal', aggiornaLarghezzaModal);
      $(window).on('resize', function () {
        aggiornaLarghezzaModal();
        pianificaLayoutHome();
      });

      $(document).ready(function () {
        pianificaLayoutHome();

        var schemaOutput = document.getElementById('schema_sensori');
        if (schemaOutput && window.MutationObserver) {
          var observer = new MutationObserver(function () {
            pianificaLayoutHome();
          });
          observer.observe(schemaOutput, {childList: true, subtree: true});
        }
      });

      document.addEventListener('load', function (event) {
        if (
          event.target &&
          event.target.matches &&
          event.target.matches('.schema-frame img')
        ) {
          pianificaLayoutHome();
        }
      }, true);

      $(document).on('click', '.sensor-hotspot', function () {
        Shiny.setInputValue(
          'schema_sensor_click',
          {
            sensor: $(this).attr('data-sensor'),
            nonce: Date.now()
          },
          {priority: 'event'}
        );
      });

      $(document).on('keydown', '.sensor-hotspot', function (event) {
        if (event.key === 'Enter' || event.key === ' ') {
          event.preventDefault();
          $(this).trigger('click');
        }
      });

      // Il dialog nativo resta sopra il modal Shiny senza sostituirlo.
      $(document).on('click', '.machine-layout-thumb', function () {
        var dialog = document.getElementById('machine-layout-dialog');
        if (!dialog) return;
        dialog.querySelector('img').src = this.querySelector('img').src;
        dialog.querySelector('img').alt = this.querySelector('img').alt;
        if (!dialog.open) dialog.showModal();
      });
      $(document).on('click', '.machine-guide-open', function () {
        var dialog = document.getElementById('machine-guide-dialog');
        if (!dialog) return;
        if (!dialog.open) dialog.showModal();
        $('#shiny-modal').trigger('san:guide-open');
      });
      $(document).on('click', '.machine-guide-close', function () {
        this.closest('dialog').close();
      });
      $(document).on('click', '.machine-layout-close', function () {
        this.closest('dialog').close();
      });
      document.addEventListener('keydown', function (event) {
        var dialog = document.querySelector('#machine-layout-dialog[open], #machine-guide-dialog[open]');
        if (dialog && dialog.open && event.key === 'Escape') {
          event.preventDefault();
          event.stopImmediatePropagation();
          dialog.close();
        }
      }, true);

      // Ripristina il focus al controllo di apertura, anche chiudendo con Esc.
      // Non intercetta gli eventi di Bootstrap o i messaggi reattivi di Shiny.
      if (!window.sanInteractionsBound) {
        window.sanInteractionsBound = true;
        var sanModalOpener = null;
        $(document).on('show.bs.modal.san', '#shiny-modal', function () {
          var active = document.activeElement;
          if (active && active !== document.body && !active.closest('.modal')) {
            sanModalOpener = active;
          }
        });
        $(document).on('shown.bs.modal.san', '#shiny-modal', function () {
          var title = this.querySelector('.modal-machine-title, .report-title, .san-guide-title');
          if (title) {
            title.id = 'san-dialog-title';
            this.setAttribute('aria-labelledby', title.id);
          } else {
            this.setAttribute('aria-label', 'Dettaglio macchina');
          }
        });
        $(document).on('hidden.bs.modal.san', '#shiny-modal', function () {
          window.requestAnimationFrame(function () {
            if (!document.querySelector('.modal.in, .modal.show') &&
                sanModalOpener && sanModalOpener.isConnected) {
              sanModalOpener.focus({preventScroll: true});
              sanModalOpener = null;
            }
          });
        });
      }
    "))
}
