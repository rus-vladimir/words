import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { Button } from "./ui/button";
import { useTranslation } from "react-i18next";
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@/components/ui/popover";

export function Info() {
  const { t } = useTranslation();
  return (
    <div className="">
      <Popover>
        <PopoverTrigger asChild>
          <Button
            variant="outline"
            size="icon"
            className="text-primary opacity-80 hover:text-[#ff7f50] active:text-[#ff7f50]"
          >
            <FontAwesomeIcon icon="question" size="lg" />
          </Button>
        </PopoverTrigger>

        <PopoverContent className="w-screen">
          <div>
            <h1 className="text-xl font-bold">{t("rules.game")}</h1>
            <ul>
              <li>- {t("rules.guess")}</li>
              <li>- {t("rules.rounds")}</li>
              <li>- {t("rules.length")}</li>
              <li>
                - {t("rules.accepted")}:
                <dl>
                  <dt className="text-green-700">{t("rules.green")}</dt>
                  <dd>* {t("rules.green.info")}</dd>
                  <dt className="text-orange-600">{t("rules.orange")}</dt>
                  <dd>* {t("rules.orange.info")}</dd>
                  <dt className="text-neutral-600">{t("rules.gray")}</dt>
                  <dd>* {t("rules.gray.info")}</dd>
                </dl>
              </li>
            </ul>
          </div>
        </PopoverContent>
      </Popover>
    </div>
  );
}
