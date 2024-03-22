data Point = Point{x::Int, y::Int}

data Position = Position {topLeftCorner :: Point, width :: Int, height :: Int}

data Component = TextBox {name :: String, position :: Position, text :: String} | 
                Button {name :: String, position :: Position, text :: String} | 
                Container {name :: String, children :: [Component]}

gui :: Component
gui = Container "Moje GUI" [
    Container "Menu" [
        Button "button_new" (Position (Point 10 10) 100 20) "New",
        Button "button_save" (Position (Point 10 10) 100 20) "Save"
    ],
    Container "Page" [
        TextBox "tb_1" (Position (Point 100 150) 100 20) "Muj text"
    ],
    Container "Footer" []
    ]

instance Show Component where
    show c = showComponent c 0 where 
        showComponent (Button name position text) tabs = replicate tabs '\t' ++ text ++ "\n"
        showComponent (TextBox name position text) tabs = replicate tabs '\t' ++ text ++ "\n"
        showComponent (Container name children) tabs = replicate tabs '\t' ++ name ++ "\n" ++ concatMap (\x -> showComponent x (tabs +1)) children

identity :: Component -> Component
identity (Container n children) = Container n [identity c | c <- children]
identity (TextBox n p t ) = TextBox n p t
identity (Button n p t ) = Button n p t


insrtInto :: Component -> String -> Component -> Component
insrtInto (Container n children) name comp = 
    let 
        chs = if n == name then children ++ [comp] else children
    in Container n [insrtInto c name comp | c <- chs]
insrtInto (TextBox n p t ) name comp = TextBox n p t
insrtInto (Button n p t ) name comp = Button n p t

-- name == n -> add into array comp