Shiny.addCustomMessageHandler("hideTooltip", hideTooltip);
Shiny.addCustomMessageHandler("hoverTooltipHandler", showTooltip);

const plot = document.getElementById("csv_data_plot");
const plotTooltipDiv = document.getElementById("plot_tooltip");
const tip = tippy('#plot_tooltip', {
  content: 'Tooltip',
  trigger: 'manual',
  placement: 'center'
})[0];

plot.addEventListener('mouseleave', e => tip.hide())

function hideTooltip(message) {
  tip.hide();
}

function showTooltip(tooltipData) {
  console.log(tooltipData);
  plotTooltipDiv.style.top = `${tooltipData["y"]}px`;
  plotTooltipDiv.style.left = `${tooltipData["x"]}px`;
  tip.setContent(tooltipData['content']);
  tip.show();
}