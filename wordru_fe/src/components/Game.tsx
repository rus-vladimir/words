import React, { FormEvent, useEffect, useState } from "react";
import useFocus from "../api/config/utilHooks";
import GameResult from "../api/core";
import Keyboard from "./Keyboard";
import Word from "./Word";
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
    
    const updateInputValue = (evt:any, props:GameProps) => {
        const val = evt.target.value;
        if (val.length <= props.Game.gcomplexity) {
            setTextInput(val);
        }
    }

    const onSumbit = (e: FormEvent<HTMLFormElement>, props: GameProps) => {
        e.preventDefault();

        setTextInput("");
        setInputFocus();
        localStorage.removeItem(props.Game.gid+'text');
        props.OnSubmit(getTextInput);
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
                    <input type="text" autoFocus ref={inputRef} value={getTextInput} onChange={evt => updateInputValue(evt, props)}/>
                    <input type="submit" value="Submit" />
                </form>
                <br/>
                <Keyboard Game={props.Game} ></Keyboard>
            </React.Fragment>
            }

        </React.Fragment>
        );
    }

export default Game;