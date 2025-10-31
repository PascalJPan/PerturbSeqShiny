info_sheet_ui <- function(
    background_color_button = "#F5F7FF",
    text_color_button       = "#333",
    sheet_shadow_color      = "rgba(0,0,0,0.15)"
) {
  css <- glue::glue(
    " .info-btn {
    display:inline-flex; align-items:center; gap:6px;
    padding:6px 10px; border-radius:9999px;
    border:1px solid #D0D5FF; background:[[background_color_button]];
    color:[[text_color_button]]; font-weight:500;
    cursor:pointer; box-shadow:0 2px 6px rgba(0,0,0,.06);
    transition: background .2s ease, box-shadow .2s ease, filter .2s ease;
  }
  .info-btn:hover {
    filter: brightness(1.1);
    box-shadow: 0 3px 8px rgba(0,0,0,.08);
  }

  #info-sheet {
    position:fixed; left:15px; right:15px; bottom:0;
    transform:translateY(110%); opacity:0;
    transition:transform .25s ease, opacity .25s ease;
    background:#fff; border:1px solid #e6e6e6;
    border-radius:14px 14px 0 0;
    box-shadow:0 -8px 28px [[sheet_shadow_color]];
    padding:16px 16px 10vh 16px; z-index:1050;
  }
  #info-sheet.show { transform:translateY(0); opacity:1; }
  #info-sheet .info-sheet-close {
    position:absolute; top:10px; right:10px;
    border:none; background:transparent; font-size:18px;
    cursor:pointer; line-height:1; color:#666;
  }
  #info-sheet .info-sheet-title {
    font-weight:600; margin:6px 28px 8px 2px;
  }
  #info-sheet .info-sheet-text { color:#333; }",
    .open = "[[", .close = "]]"
  )
  
  tagList(
    singleton(tags$head(tags$style(HTML(css)))),
    tags$div(
      id = "info-sheet",
      tags$button(class = "info-sheet-close", id = "info-sheet-close", "×"),
      tags$div(class = "info-sheet-title", id = "info-sheet-title"),
      tags$div(class = "info-sheet-text",  id = "info-sheet-text")
    ),
    singleton(tags$script(HTML("
      (function(){
        const sheet   = document.getElementById('info-sheet');
        const titleEl = document.getElementById('info-sheet-title');
        const textEl  = document.getElementById('info-sheet-text');
        const closeEl = document.getElementById('info-sheet-close');

        if (closeEl) closeEl.addEventListener('click', function(){
          sheet && sheet.classList.remove('show');
        });

        Shiny.addCustomMessageHandler('show-info-sheet', function(payload){
          if (!sheet) return;
          titleEl.textContent = payload.title || '';
          if (payload.html) { textEl.innerHTML = payload.html; }
          else { textEl.textContent = payload.text || ''; }
          sheet.classList.add('show');
        });

        Shiny.addCustomMessageHandler('hide-info-sheet', function(){
          sheet && sheet.classList.remove('show');
        });

        // (Optional legacy binding kept, safe to keep or remove)
        Shiny.addCustomMessageHandler('bind-info-click', function(sel){
          const el = document.getElementById(sel);
          if (!el) return;
          if (el._infoBound) return;
          el._infoBound = true;
          el.addEventListener('click', function(){
            Shiny.setInputValue(sel + '_clicked', Math.random(), {priority: 'event'});
          });
        });
      })();
    ")))
  )
}
