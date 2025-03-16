# Install required packages if needed
# install.packages("DiagrammeR")
library(DiagrammeR)

# Create the Mermaid diagram
mermaid_diagram <- "
flowchart TD
    subgraph DataLoaderModule[Data Loader Module]
        fileInput[File Input UI] --> uploadEvent[Upload Event Handler]
        clearBtn[Clear Button] --> clearEvent[Clear Event Handler]

        uploadEvent --> validateCols{Validate Columns}
        validateCols -->|Valid| rv[reactiveVal: rv]
        validateCols -->|Invalid| showError[Show Error Notification]

        clearEvent --> resetFile[Reset File Input]
        clearEvent --> clearRV[Clear rv]
        clearEvent --> showClearMsg[Show Clear Notification]

        rv --> validData[reactive: valid_data]
        validData --> validateData{Validate Data}
        validateData -->|Valid| returnData[Return Data]
        validateData -->|Invalid| showValidateError[Show Validation Error]
    end

    subgraph MainApp[Main Application]
        returnData --> observe[observe Block]
        observe --> |Data NULL| clearOutputs[Clear Outputs]
        observe --> |Data Present| renderInfo[Render Data Info]
        observe --> |Data Present| renderPreview[Render Data Preview]

        clearOutputs --> emptyTable[Empty Table]
        clearOutputs --> emptyInfo[Empty Info]

        renderInfo --> dataInfo[Data Info Output]
        renderPreview --> dataPreview[Data Preview Output]
    end

    style DataLoaderModule fill:#f0f0f0,stroke:#333,stroke-width:2px
    style MainApp fill:#e6e6e6,stroke:#333,stroke-width:2px
"

# Render the diagram
DiagrammeR::mermaid(mermaid_diagram)

# To save the diagram as an HTML file (optional)
# library(htmlwidgets)
# saveWidget(DiagrammeR::mermaid(mermaid_diagram), "reactive_flow_diagram.html")

# Alternative approach using DiagrammeR's native graph syntax
# This gives you more programmatic control over the diagram

DiagrammeR::grViz("
  digraph reactive_flow {
    graph [rankdir = TD, compound = true]

    subgraph cluster_0 {
      label = 'Data Loader Module'
      style = filled
      color = gray90

      fileInput [label = 'File Input UI']
      uploadEvent [label = 'Upload Event Handler']
      validateCols [label = 'Validate Columns', shape = diamond]
      rv [label = 'reactiveVal: rv']
      showError [label = 'Show Error']

      fileInput -> uploadEvent
      uploadEvent -> validateCols
      validateCols -> rv [label = 'Valid']
      validateCols -> showError [label = 'Invalid']
    }

    subgraph cluster_1 {
      label = 'Main Application'
      style = filled
      color = gray85

      observe [label = 'observe Block']
      renderInfo [label = 'Render Info']
      renderPreview [label = 'Render Preview']

      observe -> renderInfo
      observe -> renderPreview
    }

    rv -> observe [lhead = cluster_1]
  }
")
