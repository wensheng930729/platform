package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 
 * </p>
 *
 * @author liliang
 * @since 2019-04-28
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "用户管理列表dto")
public class UserManagerListDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("用户id")
    private Integer id;

    @ApiModelProperty("用户姓名")
    private String nickname;

    @ApiModelProperty("手机号")
    private String phone;

    @ApiModelProperty("注册时间")
    private Date createAt;

    @ApiModelProperty("关联公司与账号状态")
    private List<UserEnterpriseDTO> linkEnterprise;


}
