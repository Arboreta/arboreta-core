There are a few different approaches to making a useable window system for laying out stuff.
Here's the main styles:

lambda style - each window objects gets a single function slot, `paint`, which is called to repaint its area. 
This is mildly elegant, and very easy to implement, but also very slow, because everything has to be redrawn every frame.
That is, unless we make some ad-hoc way of detecting which things need to be updated, which is sketchy at best.
It is extremely flexible, though, and makes it easy to integrate normal data types.

```
;; the function here is called every frame
(define-subwindow root-window titlebar
   (lambda (window)
      (inherit x window)
      (inherit y window)
      (inherit width window)
      (set-attribute height window 20)
      (draw-centered-text (attribute title window))))
```

declaratory - each window has a single type, and some collection of attributes which describe how to render it,
and how it changes in predefiend ways in relation to other variables around it, so those calculations can be highly optimized.
In its most extreme form, external data is simply not allowed inline, and you have to change the tree and redraw in order to
change the content or style. This approach is likely to be very fast, since it's just optimized operations on data, and
redrawing is done only when necessary. This is quite rigid, and can be verbose.

```
;; since the type is text, this is only redrawn if the window is repositioned, or the content changes
(define-subwindow root-window titlebar
   :height 20
   :type 'text
   :layout 'center
   :content "window title")
```

hybrid - close to declaratory, but you can use normal dynamic values, and must specify under what conditions a redraw should occur.
