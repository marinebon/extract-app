# extract-app
Application (using R Shiny) for setting up extraction of ERDDAP datasets over space/time/taxa/variables

See also:
* [notes | mbon extract-app - Google Docs](https://docs.google.com/document/d/1atYZxBg7MVbyt1RX4hDgQdgZGS0uJuNdrP37vnRsxXw/edit)
* [Areas of Interest (AOI)](https://mikejohnson51.github.io/AOI/) R package

## html

These web pages (\*.html) are typically rendered from Rmarkdown (\*.Rmd):

<!-- Jekyll rendering: -->
{% for file in site.static_files %}
  {% if file.extname == '.html' %}
* [{{ file.basename }}]({{ site.baseurl }}{{ file.path }})
  {% endif %}
{% endfor %}
