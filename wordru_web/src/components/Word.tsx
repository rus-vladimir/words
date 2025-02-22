import { Component } from "react";
import GameResult from "../api/core";
import Letter from "./Letter";
import { LetterPresence } from "./Core";

interface WordProps {
  Game: GameResult;
}

interface WordState {}

const merge = (originalString: string, resultString: string) => {
  // console.log(originalString, resultString);
  if (originalString && resultString) {
    return [...originalString].map((c, ci: number) =>
      resultString[ci] == "#" ? (
        <Letter key={1000 + ci} char={c} presence={LetterPresence.Missing} />
      ) : resultString[ci] == resultString[ci].toLowerCase() ? (
        <Letter
          key={1000 + ci}
          char={c}
          presence={LetterPresence.WrongPossition}
        />
      ) : (
        <Letter
          key={1000 + ci}
          char={c}
          presence={LetterPresence.CorrectPosition}
        />
      ),
    );
  }
};

class Word extends Component<WordProps, WordState> {
  state = { Chars: String };
  render() {
    var rounds = this.props.Game.grounds;
    return (
      <div className="flex-center flex-col space-y-0.5 sm:space-y-1">
        {rounds.length > 0 &&
          rounds.map((e: [string], ri: number) => (
            <div
              className="flex-center space-x-0.5 sm:space-x-1"
              key={7000 + ri}
            >
              {merge(e.at(0)!, e.at(1)!)}
            </div>
          ))}
      </div>
    );
  }
}

export default Word;
