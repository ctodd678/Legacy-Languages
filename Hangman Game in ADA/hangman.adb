-- CIS*3190 Assignment 2 - Connor Todd

with Text_IO; use Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO; 
with ada.numerics.discrete_random;



procedure Hangman is
type dictArray is array(1..50) of Unbounded_String;
type rand_range is range 1..50;
type charArray is array(1..30) of Character;
package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
seed : Rand_Int.Generator;      --seed for random number
Num : rand_range;               --range used for random nubmers
dict: dictArray;                --array of all the words in the game
--randInt : Integer;              --used to to get a random number
wordLength : Integer;           --for length of the word
numGuesses : Integer;           --number of guesses
nCorrGuesses : Integer;         --number of correct guesses
--Last : Integer;                 --used for string input
word : Unbounded_String;        --the string of the word
guessWord : Unbounded_String;   --string of the word the user guessed
--test : String := "Test"; 
currGuss : Character := '.';    --used to hold the current guess letter
continue : Character := 'Y';    --Y/N value to keep playing game
guesses : charArray;            --used to hold all guesses made by user 
usedLetters : charArray;        --used to hold all guesses made by user
correctArray : charArray;       --used to hold the word seperated into characters for easier comparison
guessTruthValue : Boolean;      --True -> letter is in worod, False -> letter is not in word
usedGuessTruthVal : Boolean;    --
wordTruthValue : Boolean;       --True -> correct word guessed, False -> incorrect word guessed
void : Integer;                 --to make a "fake" void function

--This function prints the corresponding hangman image
function hangmanImage (Var : in Integer) return Integer is
begin
    if Var = 0 then
        Put_Line("XXXXXXX");
        Put_Line("X     X");
        Put_Line("X");
        Put_Line("X");
        Put_Line("X");
        Put_Line("X");
        Put_Line("X");
        Put_Line("X");
        Put_Line("X");
        Put_Line("X");
        Put_Line("X");
    elsif Var = 1 then
        Put_Line("First we draw a head.");
        Put_Line("XXXXXXX");
        Put_Line("X     X");
        Put_Line("X    ---");
        Put_Line("X   (. .)  ");
        Put_Line("X    ---");
        Put_Line("X");
        Put_Line("X");
        Put_Line("X");
        Put_Line("X");
        Put_Line("X");
        Put_Line("X");
    elsif Var = 2 then
        Put_Line("Now we draw a body.");
        Put_Line("XXXXXXX");
        Put_Line("X     X");
        Put_Line("X    ---");
        Put_Line("X   (. .) ");
        Put_Line("X    ---  ");
        Put_Line("X     X  ");
        Put_Line("X     X");
        Put_Line("X     X");
        Put_Line("X     X");
        Put_Line("X");
        Put_Line("X");
    elsif Var = 3 then
        Put_Line("Next we draw an arm.");
        Put_Line("XXXXXXX");
        Put_Line("X     X");
        Put_Line("X    ---");
        Put_Line("X   (. .)  ");
        Put_Line("X    --- / ");
        Put_Line("X     X /");
        Put_Line("X     X/");
        Put_Line("X     X");
        Put_Line("X     X");
        Put_Line("X        ");
        Put_Line("X        ");
    elsif Var = 4 then
        Put_Line("This time it's the other arm.");
        Put_Line("XXXXXXX");
        Put_Line("X     X");
        Put_Line("X    ---");
        Put_Line("X   (. .)  ");
        Put_Line("X  \ --- /");
        Put_Line("X   \ X /");
        Put_Line("X    \X/");
        Put_Line("X     X");
        Put_Line("X     X");
        Put_Line("X      \ ");
        Put_Line("X        ");
    elsif Var = 5 then
        Put_Line("Now, let's draw the right leg.");
        Put_Line("XXXXXXX");
        Put_Line("X     X");
        Put_Line("X    ---");
        Put_Line("X   (. .)  ");
        Put_Line("X  \ --- /");
        Put_Line("X   \ X /");
        Put_Line("X    \X/");
        Put_Line("X     X");
        Put_Line("X     X");
        Put_Line("X      \ ");
        Put_Line("X        ");
    elsif Var = 6 then
        Put_Line("This time we draw the left leg.");
        Put_Line("XXXXXXX");
        Put_Line("X     X");
        Put_Line("X    ---");
        Put_Line("X   (. .)  ");
        Put_Line("X  \ --- /");
        Put_Line("X   \ X /");
        Put_Line("X    \X/");
        Put_Line("X     X");
        Put_Line("X     X");
        Put_Line("X    / \ ");
        Put_Line("X        ");
    elsif Var = 7 then
        Put_Line("Now we put up a hang.");
        Put_Line("XXXXXXX");
        Put_Line("X     X");
        Put_Line("X    ---");
        Put_Line("X   (. .) / ");
        Put_Line("X  \ --- / ");
        Put_Line("X   \ X /");
        Put_Line("X    \X/");
        Put_Line("X     X");
        Put_Line("X     X");
        Put_Line("X    / \  ");
        Put_Line("X         ");
    elsif Var = 8 then
        Put_Line("Next the other hand.");
        Put_Line("XXXXXXX");
        Put_Line("X     X");
        Put_Line("X    ---");
        Put_Line("X \ (. .) /");
        Put_Line("X  \ --- /");
        Put_Line("X   \ X /");
        Put_Line("X    \X/");
        Put_Line("X     X");
        Put_Line("X     X");
        Put_Line("X    / \ ");
        Put_Line("X        ");
    elsif Var = 9 then
        Put_Line("Now we draw one foot.");
        Put_Line("XXXXXXX");
        Put_Line("X     X");
        Put_Line("X    ---");
        Put_Line("X \ (. .) /");
        Put_Line("X  \ --- /");
        Put_Line("X   \ X /");
        Put_Line("X    \X/");
        Put_Line("X     X");
        Put_Line("X     X");
        Put_Line("X    / \ ");
        Put_Line("X       \");
    elsif Var = 10 then
        Put_Line("Here's the other foot -- You're hung!!.");
        Put_Line("XXXXXXX");
        Put_Line("X     X");
        Put_Line("X    ---");
        Put_Line("X \ (. .) /");
        Put_Line("X  \ --- /");
        Put_Line("X   \ X /");
        Put_Line("X    \X/");
        Put_Line("X     X");
        Put_Line("X     X");
        Put_Line("X    / \ ");
        Put_Line("X   /   \");
    end if;
    
    return 1;
end hangmanImage;

--intializes the guess array to dashes
function initializeGuessArray (length : Integer) return charArray is
guessArray : charArray;
begin
    for i in 1..length loop
        guessArray(i) := '-';
    end loop;
    return guessArray;
end initializeGuessArray;

--intializes the correct word array to the letters of the word
function initializeCorrectArray (length : Integer; corrWord : Unbounded_String) return charArray is
corrArray : charArray;
begin
    for i in 1..length loop
        corrArray(i) := element(corrWord, i);
    end loop;
    return corrArray;
end initializeCorrectArray;

--prints a char array (for previous guesses)
function printCharArray (index : Integer; chars : in charArray) return Integer is
void : Integer;
begin
    void := 1;

    for i in 1..index loop
        Put(chars(i) & ",");
    end loop;
    return void;
end printCharArray;

--prints a char array
function printWordArray (index : Integer; chars : charArray) return Integer is
void : Integer;
begin
    void := 1;

    for i in 1..index loop
        Put(chars(i));
    end loop;
    return void;
end printWordArray;

--checks if char is in the array
function checkGuess (length: Integer; guess : Character; chars : in charArray; guessArray : in out charArray) return Boolean is
correct : Boolean;
begin
    correct := False;

    for i in 1..length loop
        if(guess = chars(i)) then
            correct := True;
            guessArray(i) := chars(i);
        end if;
    end loop;

    if(correct) then
        for i in 1..length loop
        Put(guessArray(i));   
        end loop;
    end if;

    return correct;
end checkGuess;

--checks if char is already in the array
function checkGuesses (length: Integer; guess : Character; guesses : in charArray) return Boolean is
usedGuess : Boolean;
begin
    usedGuess := False;

    for i in 1..length loop
        if(guess = guesses(i)) then
            usedGuess := True;
            exit;
        end if;
    end loop;

    return usedGuess;
end checkGuesses;

begin
    dict(1):=to_unbounded_string("gum");
    dict(2):=to_unbounded_string("sin");
    dict(3):=to_unbounded_string("for");
    dict(4):=to_unbounded_string("cry");
    dict(5):=to_unbounded_string("lug");
    dict(6):=to_unbounded_string("bye");
    dict(7):=to_unbounded_string("fly");
    dict(8):=to_unbounded_string("ugly");

    dict(9):=to_unbounded_string("each");
    dict(10):=to_unbounded_string("from");
    dict(11):=to_unbounded_string("work");
    dict(12):=to_unbounded_string("talk");
    dict(13):=to_unbounded_string("with");
    dict(14):=to_unbounded_string("self");

    dict(15):=to_unbounded_string("pizza");
    dict(16):=to_unbounded_string("thing");
    dict(17):=to_unbounded_string("feign");
    dict(18):=to_unbounded_string("fiend");
    dict(19):=to_unbounded_string("elbow");
    dict(20):=to_unbounded_string("fault");

    dict(21):=to_unbounded_string("dirty");
    dict(22):=to_unbounded_string("budget");
    dict(23):=to_unbounded_string("spirit");
    dict(24):=to_unbounded_string("quaint");
    dict(25):=to_unbounded_string("maiden");

    dict(26):=to_unbounded_string("escort");
    dict(27):=to_unbounded_string("pickax");
    dict(28):=to_unbounded_string("example");
    dict(29):=to_unbounded_string("tension");
    dict(30):=to_unbounded_string("quinine");

    dict(31):=to_unbounded_string("kidney");
    dict(32):=to_unbounded_string("replica");
    dict(33):=to_unbounded_string("sleeper");
    dict(34):=to_unbounded_string("triangle");

    dict(35):=to_unbounded_string("kangaroo");
    dict(36):=to_unbounded_string("mahogany");
    dict(37):=to_unbounded_string("sergeant");
    dict(38):=to_unbounded_string("sequence");

    dict(39):=to_unbounded_string("moustache");
    dict(40):=to_unbounded_string("dangerous");
    dict(41):=to_unbounded_string("scientist");
    dict(42):=to_unbounded_string("different");

    dict(43):=to_unbounded_string("quiescent");
    dict(44):=to_unbounded_string("magistrate");
    dict(45):=to_unbounded_string("erroneously");

    dict(46):=to_unbounded_string("loudspeaker");
    dict(47):=to_unbounded_string("phytotoxic");
    dict(48):=to_unbounded_string("matrimonial");

    dict(49):=to_unbounded_string("parasympathomimetic");
    dict(50):=to_unbounded_string("thigmotropism");




    Put_Line("THE GAME OF HANGMAN");
    --MAIN LOOP FOR THE PROGRAM, CONTINUES UNTIL USER DECIDES TO STOP PLAYING
    while continue = 'Y' loop

        --BEGINNING OF GAME

        --get random number
        Rand_Int.Reset(seed);
        Num := Rand_Int.Random(seed);

        --get random word from dictionary
        word := dict(integer(num));
        wordLength := length(word);

        --Put_Line(word); --comment this out later
        --Put_Line(integer'image(wordLength));

        --initializes variables and arrays for each iteration of the game
        numGuesses := 0;
        nCorrGuesses := 0;
        void := 0;
        usedGuessTruthVal := False;
        guessTruthValue := False;
        guesses := initializeGuessArray (wordLength);
        usedLetters := initializeGuessArray (wordLength);
        correctArray := initializeCorrectArray(wordLength, word);
        void := printWordArray (wordLength, guesses);
        Put_Line("");

        void := void + 1;
        


        --LOOP FOR EACH ROUND OF THE GAME
        while numGuesses < 20 loop

            if(nCorrGuesses = wordLength) then
                Put_Line("Ending game...");
                exit;
            elsif(numGuesses > 10) then
                Put_Line("Sorry, you lose. The word was " & word);
                Put_Line("Do you want another word? (Y/N)");
                Get(continue);
                exit;
            end if;


            Put_Line("What is your guess?");
            Get(currGuss);
            usedGuessTruthVal := checkGuesses (numGuesses, currGuss, usedLetters);

            if(usedGuessTruthVal) then
                Put_Line("You guessed that letter before");
            else
                --Put_Line("numGuesses: " & integer'image(numGuesses));
                guessTruthValue := checkGuess (wordLength, currGuss, correctArray, guesses);
                Put_Line("");

                Put_Line("What is your guess for the word?");
                Skip_Line;
                Get_Line(guessWord);

                if(length(guessWord) = wordLength) then
                    if(guessWord = word) then
                        wordTruthValue := True;
                    else
                        wordTruthValue := False;
                    end if;
                else
                    wordTruthValue := False;
                    Put_Line("Please enter a guess that is" & integer'image(wordLength) & " letters long.");
                end if;
                

                if(wordTruthValue) then
                    numGuesses := 11;
                    Put_Line("You found the word.");
                    Put_Line("Do you want another word? (Y/N)");
                    Get(continue);
                    exit;
                else
                    if(guessTruthValue) then
                        nCorrGuesses := nCorrGuesses + 1;
                    else
                        Put_Line("Sorry, that letter isn't in the word.");
                        void := hangmanImage(numGuesses);
                    end if;

                    Put_Line("");
                end if;
                usedLetters(numGuesses + 1) := currGuss;
                numGuesses := numGuesses + 1; --increment guess counter
            end if;

            
            Put_Line("Here are the letters you used:");
            void := printCharArray (numGuesses, usedLetters);
            Put_Line("");

        end loop;

    end loop;
    
end Hangman;