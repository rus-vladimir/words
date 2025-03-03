import React, { FormEvent, useEffect, useState } from "react";
import useFocus from "../api/config/utilHooks";
import GameResult from "../api/core";
import Keyboard from "./Keyboard";
import Word from "./Word";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { useTranslation } from "react-i18next";
import { Button } from "@/components/ui/button";
import {
  InputOTP,
  InputOTPGroup,
  InputOTPSlot,
} from "@/components/ui/input-otp";

interface GameProps {
  Game: GameResult;
  OnSubmit: (word: string) => void;
  Language: string;
  HasError: boolean;
}

const Game: React.FC<GameProps> = (props) => {
  const [getTextInput, setTextInput] = useState("");
  const [inputRef, setInputFocus] = useFocus();
  const { t } = useTranslation();

  useEffect(() => {
    setTextInput(localStorage.getItem(props.Game.gid + "text") ?? "");
  }, []);

  useEffect(() => {
    if (props.Game.gstatus.startsWith("Finished")) {
      localStorage.removeItem(props.Game.gid + "text");
    }
  }, [props.Game.gstatus]);

  useEffect(() => {
    localStorage.setItem(props.Game.gid + "text", getTextInput);
  }, [getTextInput]);

  const handleOnChange = (evt: any) => {
    const val = evt;
    updateInputValue(val);
  };
  const updateInputValue = (newValue: string) => {
    if (newValue.length <= props.Game.gcomplexity) {
      setTextInput(newValue);
    }
  };

  const onSumbit = (e: FormEvent<HTMLFormElement>, _: GameProps) => {
    e.preventDefault();

    handleSubmit();
  };

  function handleSubmit() {
    setTextInput("");
    if (document.activeElement === inputRef.current) setInputFocus();
    localStorage.removeItem(props.Game.gid + "text");
    props.OnSubmit(getTextInput);
  }

  const onKeyboardButtonClick = (str: string) => {
    updateInputValue(getTextInput + str);
  };

  useEffect(() => {
    function handleKeyDown(this: Document, ev: KeyboardEvent) {
      switch (ev.code) {
        case "Backspace":
          handleDeleteFromEnd();
          ev.preventDefault();
          setInputFocus();
          break;
        case "Enter":
          handleSubmit();
          ev.preventDefault();
          setInputFocus();
          break;
        case "Delete":
          handleDeleteFromStart();
          ev.preventDefault();
          setInputFocus();
          break;
        default:
          if (ev.key.length === 1) {
            updateInputValue(getTextInput + ev.key);
            ev.preventDefault();
            setInputFocus();
          }
      }
    }

    document.addEventListener("keydown", handleKeyDown);

    // Don't forget to clean up
    return function cleanup() {
      document.removeEventListener("keydown", handleKeyDown);
    };
  }, [getTextInput]);

  const handleDeleteWordClick = () => {
    updateInputValue("");
  };

  const handleDeleteLetterClick = () => {
    handleDeleteFromEnd();
  };

  function handleDeleteFromEnd() {
    var newText = getTextInput.slice(0, -1);
    updateInputValue(newText);
  }

  function handleDeleteFromStart() {
    var newText = getTextInput.slice(0, -1);
    updateInputValue(newText);
  }

  return (
    <React.Fragment>
      {<Word Game={props.Game} />}

      {props.Game.gstatus === "Finished Win" && (
        <div className="flex-center mt-20 text-2xl text-green-800">
          {t("game.won")}
          <span className="uppercase">&nbsp;{props.Game.gcorrectWord}</span>
        </div>
      )}
      {props.Game.gstatus === "Finished Lose" && (
        <div className="flex-center mt-20 text-2xl text-red-800">
          {t("game.lost")}
          <span className="uppercase">&nbsp;{props.Game.gcorrectWord}</span>
        </div>
      )}
      {!props.Game.gstatus.startsWith("Finished") && (
        <React.Fragment>
          <div className="text-primary mt-1 text-center text-3xl font-semibold">
            {t("game.round")} <span>{props.Game.grounds.length + 1}</span>
          </div>
          <form
            className="flex flex-col items-center justify-center space-y-3"
            onSubmit={(e) => onSumbit(e, props)}
          >
            <div className={props.HasError ? `bg-red-200` : ``}>
              <InputOTP
                autoFocus
                ref={inputRef}
                value={getTextInput}
                inputMode="text"
                onChange={(evt) => handleOnChange(evt)}
                maxLength={props.Game.gcomplexity}
              >
                <InputOTPGroup>
                  {Array.from({ length: props.Game.gcomplexity }).map(
                    (_, i) => {
                      return (
                        <InputOTPSlot
                          className="size-8 font-bold capitalize sm:size-10 lg:size-14"
                          key={i}
                          index={i}
                        />
                      );
                    },
                  )}
                </InputOTPGroup>
              </InputOTP>
            </div>
            <div className="flex space-x-2">
              <Button
                variant="outline"
                size="icon"
                type="button"
                onClick={handleDeleteWordClick}
              >
                <FontAwesomeIcon icon="broom" />
              </Button>
              <Button
                type="submit"
                variant="outline"
                className="w-48 text-3xl font-semibold tracking-tight shadow-lg"
              >
                {t("game.submit")}
              </Button>
              <Button
                type="button"
                variant="outline"
                size="icon"
                onClick={handleDeleteLetterClick}
              >
                <FontAwesomeIcon icon="delete-left" />
              </Button>
            </div>
          </form>
          <div className="mt-2">
            <Keyboard
              Game={props.Game}
              OnClick={onKeyboardButtonClick}
              Language={props.Language}
            ></Keyboard>
          </div>
        </React.Fragment>
      )}
    </React.Fragment>
  );
};

export default Game;
