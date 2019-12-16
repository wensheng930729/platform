package com.bee.platform.user.vo;

import com.bee.platform.common.entity.Page;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;
import org.hibernate.validator.constraints.Range;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @description: 用户管理列表查询参数
 * @author: junyang.li
 * @create: 2019-03-20 13:55
 **/
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("用户管理列表查询参数")
public class UserManagementVO implements Serializable {

    private static final long serialVersionUID = 7379057799061103145L;

    @ApiModelProperty("搜索类型关键词")
    @Length(max = 30,message = "搜索关键词限制30个字符")
    private String  keyWord;

    @ApiModelProperty("分页对象")
    @NotNull(message = "分页对象不能为空")
    private  Page page;
}
