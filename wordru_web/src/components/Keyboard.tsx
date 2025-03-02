import React from "react";
import GameResult from "../api/core";
import { Button } from "./ui/button";
import { getLetterColor, LetterPresence } from "./Core";

interface KeyboardProps {
  Game: GameResult;
  OnClick: (char: string) => void;
  Language: string;
}

const CyrillicLetters = [
  ["а", "б", "в", "г", "д", "е"],
  ["ж", "з", "и", "й", "к", "л"],
  ["м", "н", "о", "п", "р", "с"],
  ["т", "у", "ф", "х", "ц", "ч", "ш"],
  ["щ", "ъ", "ы", "ь", "э", "ю", "я"],
];

const LatinLetters = [
  ["A", "B", "C", "D", "E"],
  ["F", "G", "H", "I", "J"],
  ["K", "L", "M", "N", "O"],
  ["P", "Q", "R", "S", "T"],
  ["U", "V", "W", "X", "Y", "Z"],
];

const RomanianLetters = [
  ["A", "Ă", "Â", "B", "C", "D"],
  ["E", "F", "G", "H", "I", "Î"],
  ["J", "K", "L", "M", "N", "O"],
  ["P", "Q", "R", "S", "Ș", "T"],
  ["Ț", "U", "V", "W", "X", "Y", "Z"],
];

var langMap = {
  en: LatinLetters,
  nl: LatinLetters,
  ru: CyrillicLetters,
  ro: RomanianLetters,
};

interface CharPresence {
  char: string;
  presense: LetterPresence;
}

const Keyboard: React.FC<KeyboardProps> = (props) => {
  const merge = (
    originalString: string,
    resultString: string,
  ): CharPresence[] => {
    if (
      originalString &&
      resultString &&
      originalString.length == resultString.length
    ) {
      // console.log(originalString, resultString);
      const charPresense = [...originalString].map((ch, ci: number) => {
        let charPresence = { char: ch } as CharPresence;
        let presense =
          resultString[ci] == "#"
            ? LetterPresence.Missing
            : resultString[ci] == resultString[ci].toLowerCase()
              ? LetterPresence.WrongPossition
              : LetterPresence.CorrectPosition;
        charPresence.presense = presense;
        return charPresence;
      });
      return charPresense;
    }
    return [];
  };

  const getLettersStatus = () => {
    var introducedLetters = props.Game.grounds.reduce(
      (a, b) => a.concat(b.at(0)!),
      "",
    );
    var checkedLetters = props.Game.grounds.reduce(
      (a, b) => a.concat(b.at(1)!),
      "",
    );
    // console.log(`introduced: ${introducedLetters}`);
    // console.log(`checked: ${checkedLetters}`);
    let mergeResult = merge(introducedLetters, checkedLetters);
    // console.log(`letterStatus ${JSON.stringify(mergeResult)}`)
    return mergeResult;
  };
  const lettersStatus = getLettersStatus();

  const getPresence = (letter: string): LetterPresence => {
    const statuses = lettersStatus
      ?.filter(
        (cp) => cp.char.toLocaleLowerCase() == letter.toLocaleLowerCase(),
      )
      .map((cp) => cp.presense);
    // console.log(`statuses for ${letter}: ${JSON.stringify(statuses)}`);
    if (statuses.some((s) => s == LetterPresence.CorrectPosition)) {
      return LetterPresence.CorrectPosition;
    }
    if (statuses.some((s) => s == LetterPresence.WrongPossition)) {
      return LetterPresence.WrongPossition;
    }
    if (statuses.some((s) => s == LetterPresence.Missing)) {
      return LetterPresence.Missing;
    }
    return LetterPresence.Empty;
  };

  const letters = (): string[][] => {
    return langMap[props.Language as keyof typeof langMap];
  };

  const getLetterClassName = (c: string) => {
    var presense = getPresence(c);
    var className = getLetterColor(presense);
    return className;
  };

  return (
    <div className="flex-center flex-col space-y-1 sm:space-y-2">
      {letters().map((row, ri) => (
        <div className="flex-center space-x-1 sm:space-x-2" key={(ri + 1) * 10}>
          {row.map((l, li) => (
            <Button
              key={(ri + 1) * 10 + (li + 1) * 5}
              onClick={() => props.OnClick(l)}
              className={`${getLetterClassName(l)} text-secondary dark:text-primary size-11 content-center p-3 text-2xl capitalize sm:p-4 sm:text-2xl sm:font-semibold md:size-11 lg:size-12`}
            >
              {l}
            </Button>
          ))}
        </div>
      ))}
    </div>
  );
};

export default Keyboard;
