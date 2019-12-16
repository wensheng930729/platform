package com.bee.platform.user.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName AppAbbreviationType
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/12$ 10:00$
 * @version 1.0.0
 */

@Getter
@AllArgsConstructor
@NoArgsConstructor
public enum AppAbbreviationType {

    SCF("SCF","领蜂供应链"),
    WSCF("WSCF","领蜂供应链体验版"),
    IIOT("IIOT","蜂创物联"),
    WL("WL","集蜂联运"),
    TRD("TRD","线上蜂贸"),
    ERP("ERP","金蜜工业大脑"),
    ERPSI("ERPSI","金蜜工业大脑-硅系"),
    ERPDINAS("ERPDINAS","金蜜工业大脑-砂石"),
    EKP("EKP","金蜜OA"),
    PLAT("PLAT","工业云平台");

    private String code;
    private String name;

}
