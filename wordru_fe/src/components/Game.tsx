import React, { FormEvent, useEffect, useState } from "react";
import useFocus from "../api/config/utilHooks";
import GameResult from "../api/core";
import Keyboard from "./Keyboard";
import Word from "./Word";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
interface GameProps {
    Game: GameResult,
    OnSubmit: any
}

const Game : React.FC<GameProps> = (props) =>{

    const [getTextInput, setTextInput] = useState('');
    const [inputRef, setInputFocus] = useFocus()

    useEffect(() => {
        setTextInput(localStorage.getItem(props.Game.gid+'text') ?? '');
    }, []);

    useEffect(() => {
        if (props.Game.gstatus.startsWith("Finished"))
        {
            localStorage.removeItem(props.Game.gid+'text');
        }
    }, [props.Game.gstatus]);

    useEffect(() => {
        localStorage.setItem(props.Game.gid+'text', getTextInput);
    }, [getTextInput]);

    const handleOnChange = (evt:any) => {
        const val = evt.target.value;
        updateInputValue(val);
    }
    const updateInputValue = (newValue:string) => {
        if (newValue.length <= props.Game.gcomplexity) {
            setTextInput(newValue);
        }
    }

    const onSumbit = (e: FormEvent<HTMLFormElement>, props: GameProps) => {
        e.preventDefault();

        setTextInput("");
        setInputFocus();
        localStorage.removeItem(props.Game.gid+'text');
        props.OnSubmit(getTextInput);
    }

    const onKeyboardButtonClick = (str: string) => {
        updateInputValue(getTextInput + str);
    }

    const handleDeleteWordClick = () => {
        updateInputValue("");
    }

    const handleDeleteLetterClick = () => {
        updateInputValue(getTextInput.slice(0, -1));
    }
    

    return (
        <React.Fragment>

            {<Word Game={props.Game}></Word>}
            
            {props.Game.gstatus === "Finished Win" && <h1>YOU WON! Word: {props.Game.gcorrectWord}</h1>}
            {props.Game.gstatus === "Finished Lose" && <h1>YOU LOST! Word: {props.Game.gcorrectWord}</h1>}
            {!props.Game.gstatus.startsWith("Finished") && 
            <React.Fragment>
                <h1>Round <span>{props.Game.grounds.length+1}</span></h1>
                <form onSubmit={(e) => onSumbit(e, props)}>
                <div className="field has-addons">
                    <div className="control">
                        <button type="button" className="button is-clickable" onClick={handleDeleteWordClick}>
                            <FontAwesomeIcon icon="broom" />
                        </button>
                    </div>
                    <div className="control has-icons-left has-icons-right">
                        <input className="input" placeholder="" type="text" autoFocus ref={inputRef} value={getTextInput} onChange={evt => handleOnChange(evt)}/>
                    </div>
                    <div className="control">
                        <button type="button" className="button is-clickable" onClick={handleDeleteLetterClick}>
                            <FontAwesomeIcon icon="delete-left" />
                        </button>
                    </div>
                    <div className="control">
                        <button type="submit" className="button is-info">Submit</button>
                    </div>
                    </div>
                </form>
                <br/>
                <Keyboard Game={props.Game} onClick={onKeyboardButtonClick} ></Keyboard>
            </React.Fragment>
            }

        </React.Fragment>
        );
    }

export default Game;