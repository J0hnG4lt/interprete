bot : interprete_bot.hs LexBot.hs
	ghc -o bot interprete_bot.hs Lexbot.hs -fno-cse -fno-full-laziness

interprete_bot.hs : interprete_bot.y
	happy interprete_bot.y --ghc

LexBot.hs : LexBot.x
	alex -g LexBot.x -o Lexbot.hs
