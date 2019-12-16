package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;
import java.io.Serializable;

/**
 * @description: 修改用户信息
 * @author: junyang.li
 * @create: 2019-03-14 11:20
 **/
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("修改用户信息")
public class EditUserRQ implements Serializable {

    private static final long serialVersionUID = 5063746873212299279L;
    @ApiModelProperty("姓名")
    @NotEmpty(message = "姓名不能为空")
    @Length(max = 20,message = "姓名限制20个字符")
    private String nickname;

    @ApiModelProperty("用户名")
    @NotEmpty(message = "用户名不能为空")
    @Length(max = 20,message = "用户名限制20个字符")
    private String consumerName;

    @ApiModelProperty("地区")
    private String regionid;

    @ApiModelProperty("详细地址")
    private String address;

    @ApiModelProperty("QQ")
    @Pattern(regexp = "^[0-9]*$ ",message = "QQ只能为纯数字")
    @Length(max = 11,message = "QQ账号限制长度11位")
    private String qq;

    @ApiModelProperty("联系电话")
    private String fixtel;

    @ApiModelProperty("头像地址")
    private String head;

    @ApiModelProperty("邮箱")
    private  String email;

}
