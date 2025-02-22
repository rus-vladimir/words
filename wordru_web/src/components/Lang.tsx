import i18n from "i18next";
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectLabel,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";

interface LangProps {
  OnChange: (language: string) => void;
}

export function Lang(props: LangProps) {
  const onLangChange = (lang: string) => {
    i18n.changeLanguage(lang);
    props.OnChange(lang);
  };

  return (
    <div className="absolute top-0 right-0 mt-2 mr-2 w-24">
      <Select defaultValue={i18n.languages[0]} onValueChange={onLangChange}>
        <SelectTrigger className="w-24">
          <SelectValue placeholder="Lang" />
        </SelectTrigger>
        <SelectContent>
          <SelectGroup>
            <SelectItem value="en">English</SelectItem>
            <SelectItem value="ru">Русский</SelectItem>
            <SelectItem value="ro">Română</SelectItem>
            <SelectItem value="nl">Dutch</SelectItem>
          </SelectGroup>
        </SelectContent>
      </Select>
    </div>
  );
}
