package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * @author liang.li
 * @ClassName AuthEnterpriseAddRQ
 * @Description auth企业新增rq
 * @Date 2019-5-20
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class AuthEnterpriseGetDTO {

    @ApiModelProperty("公司全称")
    private String name;

    @ApiModelProperty("公司简称")
    private String simpleName;

    @ApiModelProperty("公司logo")
    private String logo;

    @ApiModelProperty("上级公司")
    private Integer pid;

}
