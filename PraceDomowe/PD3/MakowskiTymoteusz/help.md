### Accepted files
Uploaded file should be in a `.csv` format with decimal delimiter `.`, with size not exceeding 5 MB and either the `UTF-8` or `Latin-1` encoding. Any other files may or may not work with the app.


### Plot
#### Tooltip
A statically-placed tooltip appears under the plot when hovering over elements of the plot.


#### Zooming
In order to zoom in the plot an area on the plot should be selected. To zoom out double click the plot.


### Other
* There might be a slight delay in a following scenario: the `Data preview` tab is active, the `Plot!` button is pressed. Then the plot might appear with the slight delay, that is after switching to plot tab (I was unable to do this swap automatically).
* There is one hardcoded column name `...<None>...` used in code. Therefore, if a column with the same name is present in the uploaded file then there might be some unexpected results or errors.