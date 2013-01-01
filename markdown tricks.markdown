<!-- headers -->
# This is an H1
## Markdown plus h2 with a custom ID         {#id-goes-here}
[Link back to H2](#id-goes-here)
###### This is an H6


<!-- aligned math -->
$$\begin{align} \rho_{X,Y} &= \frac{cov(X,Y)}{\sigma_X\sigma_Y} \\ &= \frac{E[(X-\mu_X)(Y-\mu_Y)]}{\sigma_X\sigma_Y} \end{align}$$

<!-- table -->
A  | B | C
--- | --- | ---
1  | Male | Blue
2  | Female | Pink


<!-- quote -->
> To be, or not to be, that is the question:
> Whether 'tis nobler in the mind to suffer
> The slings and arrows of outrageous fortune,

>> nested blockquote

<!-- unordered lists (*,+,-) -->
*   Red
*   Green
*   Blue

<!-- ordered lists (the acual numbers have no effect) -->
1.  Bird
2.  McHale
3.  Parish

<!-- horizontal rules -->
* * *
***
---

<!-- automatic links -->
<http://github.com/defunkt/mustache/blob/master/lib/mustache/sinatra.rb>

<!-- internal links -->
[header][]

<!-- inline links -->
This is [an example](http://example.com/ "Title") inline link.
[This link](http://example.net/) has no title attribute.

[![Rack::Bug](http://img.skitch.com/20091027-xyf4h1yxnefpp7usyddrcmc7dn.png)]

<!-- reference style links -->
This is [an example][id] reference-style link.
This is [an example][id] reference-style link.
[id]: http://daringfireball.net/

Visit [Daring Fireball][] for more information.
[Daring Fireball]: http://daringfireball.net/

I get 10 times more traffic from [Google] [1] than from
[Yahoo] [2] or [MSN] [3].

[1]: http://google.com/        "Google"
[2]: http://search.yahoo.com/  "Yahoo Search"
[3]: http://search.msn.com/    "MSN Search"

I get 10 times more traffic from [Google][] than from
[Yahoo][] or [MSN][].

[google]: http://google.com/        "Google"
[yahoo]:  http://search.yahoo.com/  "Yahoo Search"
[msn]:    http://search.msn.com/    "MSN Search"

<!-- inline images -->
![Alt text](path_url/to/img.jpg)
![Alt text](path_url/to/img.jpg "Optional title")

<!-- reference style images -->
![Alt text][id]
[id]: path_url/to/image  "Optional title attribute"

<!-- images with dimensions -->
<img src="http://en.wikipedia.org/w/index.php?title=File:LPS.svg" alt="Smiley face" height="42" width="42" />

<!-- definition lists dictionary DOESN'T WORK -->
Apple
:   Pomaceous fruit of plants of the genus Malus in
    the family Rosaceae.
:   An american computer company.


<!-- emphasis -->
*single asterisks*
_single underscores_

<!-- strong -->
**double asterisks**
__double underscores__

<!-- inline code -->
`code --version`

<!-- code blocks, indent four spaces or use 3 back-ticks (allows syntax highlighting) -->
```javascript
 var s = "JavaScript syntax highlighting";
 alert(s);
 ```

This line has two spaces at the end (hard for you to see, but trust me!).
So this is a separate line in the *same paragraph*.