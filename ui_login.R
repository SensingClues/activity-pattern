# ui/mod_login_ui.R

mod_login_ui <- function(id, browser_path) {
  # ns <- NS(id)
  
  # Main container with set width
  div(style = "margin-left: 10px; margin-right: 10px;", 
      
      # Server selector, only shows up for test version
      if (grepl("test", browser_path)) {
        shinyWidgets::pickerInput(
          inputId = "server",
          label = "Server",
          choices = c("focus.sensingclues", "focus.test.sensingclues"),
          selected = "focus.sensingclues",
          multiple = FALSE,
          width = "100%"
        )
      },
      
      # LANGUAGE SELECTOR
      shinyWidgets::pickerInput(
        inputId = "lang",
        label   = "Select your language",
        choices = c("Dutch" = "nl", "English"= "en", "French" = "fr"),
        # c("Dutch", "English", "French", "German", "Hindi", "Romanian", "Spanish", "Swahili", "Ukrainian"),
        selected = "en",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(
          # noneSelectedText       = "Select your language...",
          liveSearch             = TRUE,
          liveSearchPlaceholder  = "Type to search..."
        ),
        width = "100%"
      ),
      br(),
      
      # Username input (full width)
      textInput(
        inputId     = "username",
        label       = "Enter your Cluey username",
        placeholder = "Username",
        width       = "100%"
      ),
      
      # Password input with toggle icon
      tags$div(style = "position: relative; margin-top: 10px;",
               # Label
               tags$label(`for` = "password", "Password"),
               # Eigen input ipv shiny::passwordInput om custom styling mogelijk te maken
               tags$input(
                 id          = "password",
                 type        = "password",
                 class       = "form-control",
                 placeholder = "Password",
                 style       = "width: 100%; padding-right: 30px;"
               ),
               # Oogje voor toggle
               tags$span(
                 class = "toggle-password",
                 tags$i(class = "fa fa-eye fa-lg"),
                 style = "position: absolute; top: 34px; right: 10px; cursor: pointer;"
               )
      ),
      
      # Remember me checkbox onder de login knop
      tags$div(
        style = "margin-top: 15px; font-size: 0.9em;",
        checkboxInput(
          inputId = "remember_me",
          label = "Remember me",
          value = FALSE,
          width = NULL
        )
      ),
      
      # Login button (full width)
      actionButton(
        inputId = "ok",
        label   = "Log in",
        class   = "btn btn-primary",
        style   = "width: 100%; color: white; background-color: #004d40;"
      ),
      
      # "Forgot password" link
      tags$a(
        href   = "https://sensingclues.freshdesk.com/support/solutions/articles/48001167031-forgot-password",
        "Forgot password",
        target = "_blank",
        style  = "display: block; text-align: right; margin-top: 8px; 
        color: #004d40; text-decoration: none; font-weight: 300;"
      ),
      
      # "Create account" link
      tags$div(
        style = "margin-top: 20px; border-top: 1px solid #ccc; padding-top: 10px; text-align: center; font-size: 0.9em; color: #666;",
        "No account yet? ",
        tags$a(
          href = "https://sensingclues.freshdesk.com/support/solutions/articles/48000944302-create-an-account",
          "Create one.",
          target = "_blank",
          style = "color: #004d40; text-decoration: none; font-weight: 500;"
        )
      ),
      
      # Script to toggle password visibility
      tags$script(HTML(
        "$(document).on('click', '.toggle-password', function() {\n",
        "  var input = $(this).siblings('input');\n",
        "  var type = input.attr('type') === 'password' ? 'text' : 'password';\n",
        "  input.attr('type', type);\n",
        "  $(this).find('i').toggleClass('fa-eye fa-eye-slash');\n",
        "});"
      )),
      
      # Script to close modal on clicking enter
      tags$script(HTML(
        ' $(document).keyup(function(event) {
          if ($("#password").is(":focus") && (event.keyCode == 13)) {
            $("#ok").click();
          }
        });'
      )),
      
      tags$script(HTML(glue::glue("
        $(document).ready(function() {{
          // Bij laden van de pagina: check localStorage
          if (localStorage.getItem('remember_me') === 'true') {{
            $('#{'remember_me'}').prop('checked', true);
            $('#{'username'}').val(localStorage.getItem('username'));
          }}

          // Bij klikken op login: onthoud gegevens als aangevinkt
          $('#{'ok'}').on('click', function() {{
            if ($('#{'remember_me'}').is(':checked')) {{
              localStorage.setItem('remember_me', 'true');
              localStorage.setItem('username', $('#{'username'}').val());
            }} else {{
              localStorage.removeItem('remember_me');
              localStorage.removeItem('username');
            }}
          }});
        }});
      "))
      )
  )
}