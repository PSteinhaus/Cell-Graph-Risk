# Cell-Graph-Risk
A small somewhat risk-like game I've been thinking about for a while now. It is based upon a special physics simulation with elastic graphs at its core. I've implemented it to learn some Rust, using the wonderful [ggez](https://github.com/ggez/ggez).

![](https://media.giphy.com/media/a3uWySQ5UcSR7wRirN/giphy-downsized-large.gif)

## Gameplay
It's a local mutliplayer strategy game where players create and cut connections between cells and then jump between them and send units across. They battle for domination over the white cells, which are able to create new units.

![](https://media.giphy.com/media/JTu43179DY5L6j4Yuk/giphy-downsized-large.gif)

### Cells
- Basic
  - able to create new connections and cells
  - also able to split
- Wall
  - easier to defend
  - can form impenetrable walls when connected to other wall cells
- Propulsion
  - can burn units to accelerate
- Cancer
  - hostile
  - slowly produces units
  - actively connects to other cells

## Controls

|Input|Effect|
--- | ---
|left stick|   jump between cells / control cursor for new connections/cells / control propulsion|
|right stick|  choose connection|
|A| create a new connection/cell OR burn units for propulsion
|B| release immediately to cut connection; hold longer to remove cell; hold even longer to abort
|X| shorten connection
|Y| lengthen connection
|LB| start sending units over connection
|RB| stop sending units over connection
|LT| increase number of units that have to be stored before additional units are sent to other cells
|RT| decrease number of units that have to be stored before additional units are sent to other cells
|digipad-UP| mutate into basic cell OR split
|digipad-RIGHT| mutate into wall cell
|digipad-LEFT| mutate into propulsion cell
|digipad-DOWN| mutate into cancer cell
|Select| change displayed information
