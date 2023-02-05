import * as React from 'react';
import { Component } from 'react';

export enum Presence {
    CorrectPosition, 
    WrongPossition,
    Missing,
    Empty
}
    

interface LetterProps {
    
    Char : string,
    Presence : Presence
}
 
interface LetterState {
    Char : string
}

const getStyle = (p: Presence) => {
    const missingStyle = {
        background: 'gray',
        display: 'inline-block',
        width: '30px',
        textTransform: "capitalize" as const
    };

    const correctStyle = {
        background: 'green',
        display: 'inline-block',
        width: '30px',
        textTransform: "capitalize" as const
    };

    const wrongStyle = {
        background: 'orange',
        display: 'inline-block',
        width: '30px',
        textTransform: "capitalize" as const
    };

    const emptyStyle = {
        background: 'black',
        display: 'inline-block',
        width: '30px',
        textTransform: "capitalize" as const
    };

    switch(p){
        case Presence.Missing:
            return missingStyle;
        case Presence.WrongPossition:
            return wrongStyle;
        case Presence.CorrectPosition:
            return correctStyle;
        case Presence.Empty:
            return emptyStyle;
        default:
            return emptyStyle!;
    }
}

 
class Letter extends Component<LetterProps, LetterState> {
    render() { 
        const char= this.props.Char;
        const styles = {
            border: '1px solid rgba(0, 0, 0, 0.5)',
            margin: '2px',
            display: 'inline-block',
            width: '35px',
        };

        const letterStyle = getStyle(this.props.Presence)
    
        return ( 
         <span style={styles}>
            {
                char 
                ?
                  <span style={letterStyle}>{char}</span>
                :
                  <span>_</span>
            }
         </span>
        );
    }
}
 
export default Letter;