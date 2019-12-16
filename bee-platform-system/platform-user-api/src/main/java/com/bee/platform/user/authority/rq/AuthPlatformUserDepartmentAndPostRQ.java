package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @Classname AuthPlatformUserDepartmentAndPostRQ
 * @Description 关联公司部门职位
 * @Date 2019/5/23 13:42
 * @Author xin.huang
 */
@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("关联公司部门职位")
public class AuthPlatformUserDepartmentAndPostRQ implements Serializable {

	private static final long serialVersionUID = 9073017604244309229L;

	@ApiModelProperty("关联公司id")
    private Integer enterpriseId;

    @ApiModelProperty("部门id")
    private Integer departmentId;

    @ApiModelProperty("职位id")
    private Integer postId;

    @ApiModelProperty("用户类型： 1企业成员 2企业管理员")
    private Integer userType = 1;
}
