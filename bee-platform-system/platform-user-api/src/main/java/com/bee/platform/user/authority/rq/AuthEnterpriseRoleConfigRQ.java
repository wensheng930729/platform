package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.validation.annotation.Validated;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName AuthEnterpriseRoleConfigRQ
 * @Description 功能描述
 * @Date 2019/5/29 9:57
 **/


@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("企业权限配置关联功能和应用配置请求参数")
public class AuthEnterpriseRoleConfigRQ implements Serializable {

    private static final long serialVersionUID = 9201784476170148394L;

    @ApiModelProperty("企业id")
    @NotNull(message = "企业id不能为空")
    private  Integer enterpriseId;

    @ApiModelProperty("返回的集合")
    private  List<EnterpriseRelationRoleRQ> list;

}
