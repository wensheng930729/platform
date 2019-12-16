package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
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
@ApiModel(value = "用户管理列表详情修改入参")
public class UserListDetailRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("用户id")
    @NotNull(message = "用户id不能为空")
    private Integer id;

    @ApiModelProperty("用户姓名")
    private String nickname;

    @ApiModelProperty("头像")
    private String head;

    @ApiModelProperty("固话")
    private String fixtel;

    @ApiModelProperty("邮箱")
    private String email;

    @ApiModelProperty("县级地区id")
    private String regionid;

    @ApiModelProperty("详细地址")
    private String address;

    @ApiModelProperty("关联公司信息")
    private List<UserListDetailEnterpriseRQ> linkEnterprises;


}
