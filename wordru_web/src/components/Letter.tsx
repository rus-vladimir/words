import { Component } from "react";
import { Button } from "./ui/button";
import { getLetterColor, LetterPresence } from "./Core";

interface LetterProps {
  char: string;
  presence: LetterPresence;
  onClick?: (char: string) => void;
}

class Letter extends Component<LetterProps> {
  handleClick = () => {
    if (this.props.onClick) this.props.onClick(this.props.char);
  };

  render() {
    return (
      <span onClick={this.handleClick}>
        <Button
          className={`${getLetterColor(this.props.presence)} size-9 content-center p-3 text-2xl font-bold capitalize sm:size-12 sm:p-4 sm:text-xl lg:text-3xl`}
        >
          {this.props.char}
        </Button>
      </span>
    );
  }
}

export default Letter;
