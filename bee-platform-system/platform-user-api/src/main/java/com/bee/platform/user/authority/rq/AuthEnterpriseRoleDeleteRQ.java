package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;

/**
 * @author liang.li
 * @ClassName AuthEnterpriseAddRQ
 * @Description auth企业角色中间表删除rq
 * @Date 2019-5-20
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class AuthEnterpriseRoleDeleteRQ {

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer id;

    @ApiModelProperty("应用ids")
    private Integer[] roles;

}
