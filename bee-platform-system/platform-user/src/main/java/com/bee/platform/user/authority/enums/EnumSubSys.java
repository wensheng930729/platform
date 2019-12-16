package com.bee.platform.user.authority.enums;

/**
 * @ClassName EnumSubSys
 * @Description 角色子系统标识
 */

public class EnumSubSys {
    public enum SubSys{
        beeSupplyChainFinance("供应链金融"),
        beePlatform("工业云平台"),
        beeIot("蜂创物联"),
        beeLogistics("集蜂联运"),
        beeTrade("线上蜂贸"),
        beeErp("金蜜ERP")
        ;

        SubSys() {
        }

        SubSys(String value) {
            this.value = value;
        }

        private String value;

        public String getValue() {
            return value;
        }
        public void setValue(String value) {
            this.value = value;
        }
    }

}
