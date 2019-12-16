package com.bee.platform.user.authority.enums;

/**
 * @author liliang
 * @date 2019-5-23
 * @ClassName EnumIndustry
 * @Description 行业枚举
 */

public class EnumIndustry {
    public enum Industry{
        commonly(0,"一般企业"),
        OfficeSupplies(1,"办公文教"),
        Electrical (2,"电工电气"),
        HardwareTools(3,"五金工具"),
        chemical(4,"化工"),
        RubberPlastic(5,"橡塑"),
        environmentalProtection(6,"环保"),
        energy(7,"能源"),
        Metallurgical (8,"冶金矿产"),
        steel(9,"钢铁"),
        ;

        Industry() {
        }

        Industry(Integer key, String value) {
            this.key = key;
            this.value = value;
        }

        private Integer key;
        private String value;

        public Integer getKey() {
            return key;
        }

        public void setKey(Integer key) {
            this.key = key;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }
    }

}
