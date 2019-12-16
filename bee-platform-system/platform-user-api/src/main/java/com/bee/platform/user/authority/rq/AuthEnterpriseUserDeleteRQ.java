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
 * @ClassName AuthEnterpriseUserDeleteRQ
 * @Description auth企业用户 中间表删除rq
 * @Date 2019-5-20
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class AuthEnterpriseUserDeleteRQ {

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer id;

    @ApiModelProperty("应用ids")
    private Integer userId;

}
