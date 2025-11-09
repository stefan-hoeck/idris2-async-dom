module Web.Canvas.Hints

import public JS
import public Web.Internal.Types

%default total

export %hint %inline
crcCanvasCompositing : (c : CanvasRenderingContext2D) => CanvasCompositing
crcCanvasCompositing = up c

export %hint %inline
crcCanvasDrawImage : (c : CanvasRenderingContext2D) => CanvasDrawImage
crcCanvasDrawImage = up c

export %hint %inline
crcCanvasDrawPath : (c : CanvasRenderingContext2D) => CanvasDrawPath
crcCanvasDrawPath = up c

export %hint %inline
crcCanvasFillStrokeStyles : (c : CanvasRenderingContext2D) => CanvasFillStrokeStyles
crcCanvasFillStrokeStyles = up c

export %hint %inline
crcCanvasFilters : (c : CanvasRenderingContext2D) => CanvasFilters
crcCanvasFilters = up c

export %hint %inline
crcCanvasImageData : (c : CanvasRenderingContext2D) => CanvasImageData
crcCanvasImageData = up c

export %hint %inline
crcCanvasImageSmoothing : (c : CanvasRenderingContext2D) => CanvasImageSmoothing
crcCanvasImageSmoothing = up c

export %hint %inline
crcCanvasPath : (c : CanvasRenderingContext2D) => CanvasPath
crcCanvasPath = up c

export %hint %inline
crcCanvasPathDrawingStyles : (c : CanvasRenderingContext2D) => CanvasPathDrawingStyles
crcCanvasPathDrawingStyles = up c

export %hint %inline
crcCanvasRect : (c : CanvasRenderingContext2D) => CanvasRect
crcCanvasRect = up c

export %hint %inline
crcCanvasShadowStyles : (c : CanvasRenderingContext2D) => CanvasShadowStyles
crcCanvasShadowStyles = up c

export %hint %inline
crcCanvasState : (c : CanvasRenderingContext2D) => CanvasState
crcCanvasState = up c

export %hint %inline
crcCanvasText : (c : CanvasRenderingContext2D) => CanvasText
crcCanvasText = up c

export %hint %inline
crcCanvasTextDrawingStyles : (c : CanvasRenderingContext2D) => CanvasTextDrawingStyles
crcCanvasTextDrawingStyles = up c

export %hint %inline
crcCanvasTransform : (c : CanvasRenderingContext2D) => CanvasTransform
crcCanvasTransform = up c

export %hint %inline
crcCanvasUserInterface : (c : CanvasRenderingContext2D) => CanvasUserInterface
crcCanvasUserInterface = up c
