package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @Classname AuthPlatformUserSelectRQ
 * @Description 平台用户条件查询传输对象
 * @Date 2019/5/21 9:23
 * @Author xin.huang
 */
@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("平台用户条件查询传输对象")
public class AuthPlatformUserSelectRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("手机号/姓名")
    private String username;

    @ApiModelProperty("公司名称")
    private String enterpriseName;

    @ApiModelProperty("公司id")
    private Integer enterpriseId;

    @ApiModelProperty("用户所在公司及子公司id")
    private List<Integer> enterpriseIds;
    
    @ApiModelProperty("部门名称")
    private String deptName;


}
