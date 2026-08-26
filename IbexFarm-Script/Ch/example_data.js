function shuffle(array) {
        for (let i = array.length - 1; i > 0; i--) {
            let j = Math.floor(Math.random() * (i + 1));
            [array[i], array[j]] = [array[j], array[i]];
        }
        return array;
    }


var init_items = [
       ['1','PréN_Connu', 'Il a visité _______________________ (他拜访了一个美丽的国家)', 'un beau pays','un pays beau', 'un beau pays'],
       ['2','PréN_Connu', 'Elle a acheté  _______________________ (她买了一个漂亮的单间公寓)','un joli studio', 'un studio joli', 'un joli studio'],
       ['3','PréN_Connu', 'Il a vu  _______________________ (他看见了一只小猫)', 'un petit chat','un chat petit', 'un petit chat'],
    
       ['4','PréN_Connu', 'Elle a acheté  _______________________ (她买了一件新产品)','un nouveau produit', 'un produit nouveau','un nouveau produit'],
       ['5','PréN_Connu', 'Il a obtenu _______________________ (他得了一个糟糕的分数）','une mauvaise note', 'une note mauvaise', 'une mauvaise note'],
       ['6','PréN_Connu', 'Elle a prononcé _______________________ (她发表了一段精彩的演讲)','un excellent discours', 'un discours excellent','un excellent discours'],
    
       ['7','PréN_NonConnu', 'Il a _______________________(他脾气很糟糕)','un fichu caractère', 'un caractère fichu', 'un fichu caractère'],
       ['8','PréN_NonConnu', 'Elle est  _______________________(她是一个差劲的舞者)', 'une piètre danseuse','une danseuse piètre', 'un piètre danseuse'],
       ['9','PréN_NonConnu', 'Il est _______________________(他是一个完美的骗子)', 'un fieffé menteur','un menteur fieffé', 'un fieffé menteur'],
    
       ['10','PréN_NonConnu', 'Il a rencontré _______________________(他碰上了一位所谓的专家)','un soi-disant expert', 'un expert soi-disant','un soi-disant expert'],
       ['11','PréN_NonConnu', 'Elle a _______________________(她有真正的天赋)', 'un véritable talent','un talent véritable', 'un véritable talent'],
       ['12','PréN_NonConnu', 'Il a commis _______________________ (他犯了许多错误） ', 'de nombreuses erreurs', 'des erreurs nombreuses','de nombreuses erreurs'],
    
       ['13','PostN_Connu', 'Il a acheté _______________________(他买了一件绿色的卫衣)', 'un vert pull','un pull vert', 'un pull vert'],
       ['14','PostN_Connu', 'Elle a mangé _______________________(她吃了一种希腊菜)', 'un grec plat','un plat grec', 'un plat grec'],
       ['15','PostN_Connu', 'Il a vu _______________________(他看到了一张圆形的桌子)', 'une ronde table','une table ronde', 'une table ronde'],
    
       ['16','PostN_Connu', 'Elle a lu _______________________(他读了一本有趣的书)', 'un intéressant livre','un livre intéressant', 'un livre intéressant'],
       ['17','PostN_Connu', 'Il est _______________________(他是一个职业音乐人)', 'un profesionnel musicien','un musicien profesionnel', 'un musicien profesionnel'],
       ['18','PostN_Connu', 'Elle a écrit _______________________(她写了一篇科学论文)','un scientifique article', 'un article scientifique','un article scientifique'],
    
       ['19','PostN_NonConnu', 'Il a acheté _______________________(他买了一块褐色面包)', 'un bis pain','un pain bis', 'un pain bis'],
       ['20','PostN_NonConnu', 'Elle a _______________________ (她声音嘶哑)', 'une suave voix','une voix suave', 'une voix suave'],
       ['21','PostN_NonConnu', 'Elle porte  _______________________(她穿了一条紫色的裙子)', 'une mauve robe ','une robe mauve', 'une robe mauve'],
       ['22','PostN_NonConnu', 'Il a pris _______________________(他修了一年的假期) ', 'une sabbatique année','une année sabbatique', 'une année sabbatique'],
       ['23','PostN_NonConnu', 'Il a chanté _______________________(他唱了一首民谣歌曲)', 'une folklorique chanson', 'une chanson folklorique','une chanson folklorique'],
       ['24','PostN_NonConnu', 'Elle a  _______________________(她有一只宠物猫)', 'un domestique chat','un chat domestique', 'un chat domestique'],
    
    
       ['101','Filler 1', 'Il évite ____ voir sa maman  (他避免见他的妈妈)', 'à','de', 'de'],
       ['102','Filler 1', 'Il tente ____ résoudre le problème  (他尝试解决这个问题)', 'à','de', 'de'],
       ['103', 'Filler 1', 'Elle essaie ____ trouver un compromis  (她尝试找到一个折中的方法)', 'à','de', 'de'],
       ['104','Filler 1', 'Elle propose ____ partager le gâteau  (她提议分享这个蛋糕)', 'à','de', 'de'],
       ['105','Filler 1', 'Elle refuse ____ vendre sa maison (她拒绝卖掉她的房子)', 'à','de', 'de'],
    
       ['106','Filler 1', 'Il apprend  ____ gérer son temps. (他学着管理他的时间)', 'à','de', 'à'],
       ['107','Filler 1', 'Il cherche  ____ financer son projet. (他试图为他的项目筹集资金)', 'à','de', 'à'],
       ['108','Filler 1', 'L’ennui conduit ____ faire d’avantage d’erreurs (厌倦会导致更多的失误)', 'à','de', 'à'],
       ['109','Filler 1', 'Elle tient ____ remercier sa maman (她坚持感谢她的妈妈)', 'à','de', 'à'],
       ['110','Filler 1', 'Elle réussit ____ faire un résumé du projet (她成功总结了研究计划)', 'à','de', 'à'],
    
       ['111','Filler 2', 'Il a lu __________ (他看了一份报纸)', 'un journal','une journal', 'un journal'],
       ['112','Filler 2', 'Il a lu __________ (他读了一首诗)', 'un poème','une poème', 'un poème'],
       ['113','Filler 2', 'Il a signé __________ (他签了一份合同)', 'un contrat','une contrat', 'un contrat'],
       ['114','Filler 2', 'Elle habite dans __________ (她住在一个村子里)', 'un village','une village', 'un village'],
       ['115','Filler 2', 'Elle a donné  __________ (她给出了一份诊断)', 'un diagnostic','une diagnostic', 'un diagnostic'],
      
       ['116','Filler 2', 'Il a acheté __________ (他买了一个蜡烛)', 'un bougie','une bougie', 'une bougie'],
       ['117','Filler 2', 'Il a pris __________ (他休息了一段时间)', 'un pause','une pause', 'une pause'],
       ['118','Filler 2', 'Elle a acheté __________ (他买了一盏台灯)', 'un lampe','une lampe', 'une lampe'],
       ['119','Filler 2', 'Elle a acheté __________ (他毁掉了一颗植物)', 'un villa','une villa', 'une villa'],
       ['120','Filler 2', 'Elle a connu __________ (他经历了一次危机)', 'un crise','une crise', 'une crise'],
       
       ['121','Filler 3','J’ai vu une personne _______ est souriante (我看到一个正在微笑的人 )', 'qui','que','qui'],
       ['122','Filler 3','J’ai lu un livre _______ est long (我读了一本很长的书)', 'qui','que','qui'],
       ['123','Filler 3','J’ai acheté un billet _______ est cher (我买了一张昂贵的车票)', 'qui','que','qui'],
       ['124','Filler 3','J’ai arrosé une plante _______ est jolie (我给一个看起来不错的植物浇水)', 'qui','que','qui'],
       ['125','Filler 3','J’ai fait un voyage _______ est long (我经历了一次漫长的旅程)', 'qui','que','qui'],
       
       ['126','Filler 3','La personne _______ j’ai vue est souriante (我看到的那个人在微笑)', 'qui','que','que'],
       ['127','Filler 3','Le livre _______ j’ai lu est long (我正在读的书很长)', 'qui','que','que'],
       ['128','Filler 3','Le billet _______ j’ai acheté est cher (我买的车票很贵)', 'qui','que','que'],
       ['129','Filler 3','La plante _______ j’ai arrosée est jolie (我浇灌的植物非常漂亮)', 'qui','que','que'],
       ['130','Filler 3','Le voyage _______ j’ai fait est long (我度过的旅程非常漫长)', 'qui','que','que'],

       
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
    newButton("<p>我已阅读同意声明并同意继续。")
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
    newText("实验已经结束。感谢您的参与！")
        .print()
    ,
    newText("您现在可以关闭此页面。")
        .print()
    ,
    // Stay on this page forever
    newButton().wait()
)