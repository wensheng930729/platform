package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * @author liang.li
 * @ClassName AuthEnterpriseDelRQ
 * @Description auth企业删除rq
 * @Date 2019-5-23
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("新权限：删除企业RQ")
public class AuthEnterpriseDelRQ {

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer id;

    @ApiModelProperty("删除原因")
    @NotEmpty(message = "删除原因不能为空")
    private String reason;

}
