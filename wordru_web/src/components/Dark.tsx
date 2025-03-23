import { SunMoon } from "lucide-react";
import { useState } from "react";

export function DarkSwitch() {
  const [dark, setDark] = useState<boolean>();
  // On page load or when changing themes, best to add inline in `head` to avoid FOUC
  document.documentElement.classList.toggle(
    "dark",
    localStorage.theme === "dark" ||
      (!("theme" in localStorage) &&
        window.matchMedia("(prefers-color-scheme: dark)").matches),
  );

  function handleOnClick(): void {
    if (dark) {
      localStorage.theme = "dark";
    } else {
      localStorage.theme = "light";
    }
    setDark(!dark);
  }

  return (
    <div>
      <SunMoon
        className="text-primary size-8 cursor-pointer opacity-80"
        onClick={() => handleOnClick()}
      />
    </div>
  );
}
