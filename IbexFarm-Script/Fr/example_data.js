function shuffle(array) {
        for (let i = array.length - 1; i > 0; i--) {
            let j = Math.floor(Math.random() * (i + 1));
            [array[i], array[j]] = [array[j], array[i]];
        }
        return array;
    }


var init_items = [
       ['1','PréN_Connu', 'Il a visité _______________________', 'un beau pays','un pays beau', 'un beau pays'],
       ['2','PréN_Connu', 'Elle a acheté  _______________________','un joli studio', 'un studio joli', 'un joli studio'],
       ['3','PréN_Connu', 'Il a vu  _______________________', 'un petit chat','un chat petit', 'un petit chat'],
    
       ['4','PréN_Connu', 'Elle a acheté  _______________________','un nouveau produit', 'un produit nouveau','un nouveau produit'],
       ['5','PréN_Connu', 'Il a obtenu _______________________','une mauvaise note', 'une note mauvaise', 'une mauvaise note'],
       ['6','PréN_Connu', 'Elle a prononcé _______________________','un excellent discours', 'un discours excellent','un excellent discours'],
    
       ['7','PréN_NonConnu', 'Il a _______________________','un fichu caractère', 'un caractère fichu', 'un fichu caractère'],
       ['8','PréN_NonConnu', 'Elle est  _______________________', 'une piètre danseuse','une danseuse piètre', 'un piètre danseuse'],
       ['9','PréN_NonConnu', 'Il est _______________________', 'un fieffé menteur','un menteur fieffé', 'un fieffé menteur'],
    
       ['10','PréN_NonConnu', 'Il a rencontré _______________________','un soi-disant expert', 'un expert soi-disant','un soi-disant expert'],
       ['11','PréN_NonConnu', 'Elle a _______________________', 'un véritable talent','un talent véritable', 'un véritable talent'],
       ['12','PréN_NonConnu', 'Il a commis _______________________', 'de nombreuses erreurs', 'des erreurs nombreuses','de nombreuses erreurs'],
    
       ['13','PostN_Connu', 'Il a acheté _______________________', 'un vert pull','un pull vert', 'un pull vert'],
       ['14','PostN_Connu', 'Elle a mangé _______________________', 'un grec plat','un plat grec', 'un plat grec'],
       ['15','PostN_Connu', 'Il a vu _______________________', 'une ronde table','une table ronde', 'une table ronde'],
    
       ['16','PostN_Connu', 'Elle a lu _______________________', 'un intéressant livre','un livre intéressant', 'un livre intéressant'],
       ['17','PostN_Connu', 'Il est _______________________', 'un profesionnel musicien','un musicien profesionnel', 'un musicien profesionnel'],
       ['18','PostN_Connu', 'Elle a écrit _______________________','un scientifique article', 'un article scientifique','un article scientifique'],
    
       ['19','PostN_NonConnu', 'Il a acheté _______________________', 'un bis pain','un pain bis', 'un pain bis'],
       ['20','PostN_NonConnu', 'Elle a _______________________', 'une suave voix','une voix suave', 'une voix suave'],
       ['21','PostN_NonConnu', 'Elle porte  _______________________', 'une mauve robe ','une robe mauve', 'une robe mauve'],
       ['22','PostN_NonConnu', 'Il a pris _______________________', 'une sabbatique année','une année sabbatique', 'une année sabbatique'],
       ['23','PostN_NonConnu', 'Il a chanté _______________________',
        'une folklorique chanson', 'une chanson folklorique','une chanson folklorique'],
       ['24','PostN_NonConnu', 'Elle a  _______________________', 'un domestique chat','un chat domestique', 'un chat domestique'],
    
    
       ['101','Filler', 'Il évite ____ voir sa maman', 'à','de', 'de'],
       ['102','Filler', 'Il tente ____ résoudre le problème', 'à','de', 'de'],
       ['103', 'Filler', 'Elle essaie ____ trouver un compromis', 'à','de', 'de'],
       ['104','Filler', 'Elle propose ____ partager le gâteau', 'à','de', 'de'],
       ['105','Filler', 'Elle refuse ____ vendre sa maison', 'à','de', 'de'],
    
       ['106','Filler', 'Il apprend  ____ gérer son temps', 'à','de', 'à'],
       ['107','Filler ', 'Il cherche  ____ financer son projet', 'à','de', 'à'],
       ['108','Filler ', 'L’ennui conduit ____ faire d’avantage d’erreurs ', 'à','de', 'à'],
       ['109','Filler ', 'Elle tient ____ remercier sa maman', 'à','de', 'à'],
       ['110','Filler ', 'Elle réussit ____ faire un résumé du projet', 'à','de', 'à'],
    
       ['111','Filler', 'Il a lu __________ ', 'un journal','une journal', 'un journal'],
       ['112','Filler', 'Il a lu __________ ', 'un poème','une poème', 'un poème'],
       ['113','Filler', 'Il a signé __________', 'un contrat','une contrat', 'un contrat'],
       ['114','Filler', 'Elle habite dans __________', 'un village','une village', 'un village'],
       ['115','Filler', 'Elle a donné  __________ ', 'un diagnostic','une diagnostic', 'un diagnostic'],
      
       ['116','Filler', 'Il a acheté __________ ', 'un bougie','une bougie', 'une bougie'],
       ['117','Filler', 'Il a pris __________ ', 'un pause','une pause', 'une pause'],
       ['118','Filler', 'Elle a acheté __________ ', 'un lampe','une lampe', 'une lamp'],
       ['119','Filler', 'Elle a acheté __________ ', 'un villa','une villa', 'une villa'],
       ['120','Filler', 'Elle a connu __________ ', 'un crise','une crise', 'une crise'],
       
       ['121','Filler','J’ai vu une personne _______ est souriante', 'qui','que','qui'],
       ['122','Filler','J’ai lu un livre _______ est long', 'qui','que','qui'],
       ['123','Filler','J’ai acheté un billet _______ est cher ', 'qui','que','qui'],
       ['124','Filler','J’ai arrosé une plante _______ est jolie', 'qui','que','qui'],
       ['125','Filler','J’ai fait un voyage _______ est long', 'qui','que','qui'],
       
       ['126','Filler','La personne _______ j’ai vue est souriante', 'qui','que','que'],
       ['127','Filler','Le livre _______ j’ai lu est long', 'qui','que','que'],
       ['128','Filler','Le billet _______ j’ai acheté est cher', 'qui','que','que'],
       ['129','Filler','La plante _______ j’ai arrosée est jolie', 'qui','que','que'],
       ['130','Filler','Le voyage _______ j’ai fait est long', 'qui','que','que'],

       
]
 
    

var items_final = shuffle(init_items);

var csv = items_final.map(function(d){
    return d.join("\t");
}).join("\n");
var csv = "Item\tCondition\tPhraseTrou\tchoixA\tchoixB\tcorrect\n"+csv+"\n";

var progressBarText = "Progression"
    
PennController.AddTable("table.csv",csv);


PennController.ResetPrefix()

Sequence("intro","choix",SendResults(), "fin");

newTrial ("intro" ,
    defaultText
        .print()
    ,
    newHtml("intro", "instruction.html")
        .log()
        .print()
    ,
    newButton("<p>J'ai lu la déclaration de consentement et j'accepte de continuer.")
        .center()
        .print()
        .wait()
)


Template("table.csv", row =>
newTrial("choix",
    newText(row.PhraseTrou+"<br><br><br><br>")
    .center()
    .print()
    ,
    newScale("echelle", row.choixA+"      ", row.choixB)
    .labelsPosition("right")
    .center()
    .print()
    .wait()
    .log("final")
    
).log("Item",row.Item)
.log("Condition",row.Condition)
.log("PhraseTrou", row.PhraseTrou)
.log("choixA", row.choixA)
.log("choixB",row.choixB)
.log("correct", row.correct)

)

// A simple final screen
newTrial ( "fin" ,
    newText("L'expérience est terminée. Merci d'avoir participé !")
        .print()
    ,
    newText("Vous pouvez maintenant fermer cette page.")
        .print()
    ,
    // Stay on this page forever
    newButton().wait()
)