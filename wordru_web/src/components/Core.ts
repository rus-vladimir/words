export enum LetterPresence {
    CorrectPosition,
    WrongPossition,
    Missing,
    Empty,
  }

export function getLetterColor(p: LetterPresence) {
  switch (p) {
    case LetterPresence.Missing:
      return "bg-neutral-600 dark:text-primary text-primary-foreground shadow-sm hover:bg-neutral-600/90 focus:bg-neutral-600/90";
    case LetterPresence.WrongPossition:
      return "bg-orange-600 dark:text-primary text-primary-foreground shadow-sm hover:bg-orange-600/90 focus:bg-orange-600/90";
    case LetterPresence.CorrectPosition:
      return "bg-green-700 dark:text-primary text-primary-foreground shadow-sm hover:bg-green-700/90 focus:bg-green-700/90";
    default:
      return "bg-stone-800 dark:text-primary text-primary-foreground shadow-sm hover:bg-stone-800/90 focus:bg-stone-800/90";
              
  }
}
