data Element = Button { name :: String, text :: String }
             | Text { text :: String }
             | Panel { children :: [Element] } deriving (Show)

panelElement :: Element 
panelElement = Panel { children = [
    Button { name = "btn_example", text = "Click" }, 
    Text { text = "smtxt" }] }

data Component = TextBox {name_:: String, text_:: String}
                | Button_ {name_:: String, value:: String}
                | Container {name_:: String, children_ :: [Component]} deriving (Show)


gui :: Component
gui = Container "My App" [
    Container "Menu" [
        Button_ "btn_new" "New",
        Button_ "btn_open" "Open",
        Button_ "btn_close" "Close"
    ],
    Container "Body" [TextBox "textbox 1" "Some text goes here"],
    Container "Footer" []]


listAllButtons :: Component -> [Component]
listAllButtons (Button_ name value) = [Button_ name value]  
listAllButtons (Container _ children) = concatMap listAllButtons children  
listAllButtons _ = []

removeAllButtons :: Component -> Component
removeAllButtons (Container name children) =
    let
        isButton :: Component -> Bool
        isButton (Button_ _ _) = False
        isButton _ = True
    in Container name ( map removeAllButtons (filter isButton children))

removeAllButtons (TextBox name value) = TextBox name value
