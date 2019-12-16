package com.bee.platform.user.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @description:
 * @author: junyang.li
 * @create: 2018-12-13 10:01
 **/
@Getter
@Setter
@NoArgsConstructor
@Accessors(chain = true)
@ToString
public class CompanyListDTO {
    /**
     * 企业名称
     */
    private List<String>  companyName;
    /**
     * 平台认证凭证
     */
    private String password;
}
