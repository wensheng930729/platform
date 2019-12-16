package com.bee.platform.customer.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "联系人相关的DTO")
public class AuthContactDto implements Serializable {
    /**
	 * 
	 */
	private static final long serialVersionUID = -5104436401175555649L;
	/**
     * 联系人id
     */
    @ApiModelProperty("联系人id")
    private Integer id;
    /**
     * 联系人编号
     */
    @ApiModelProperty("联系人编号")
    private String contactNo;
    /**
     * 二级分类
     */
    @ApiModelProperty("二级分类")
    private String secondType;
    /**
     * 联系人姓名
     */
    @ApiModelProperty("联系人姓名")
    private String name;
    /**
     * 性别
     */
    @ApiModelProperty("性别")
    private String sex;
    /**
     * 电话号码
     */
    @ApiModelProperty("电话号码")
    private String phone;
    /**
     * 座机
     */
    @ApiModelProperty("座机")
    private String fixtel;
    /**
     * 生日
     */
    @ApiModelProperty("生日")
    private String birthday;
    /**
     * 联系人的邮寄地址
     */
    @ApiModelProperty("联系人的邮寄地址")
    private String address;
    /**
     * 爱好
     */
    @ApiModelProperty("爱好")
    private String hobby;
    /**
     * 联系人星级评定：1是一星 2是二星 3是三星 4是四星 5是五星
     */
    @ApiModelProperty("联系人星级评定：1是一星 2是二星 3是三星 4是四星 5是五星")
    private Integer starLevel;
    /**
     * 工作简介
     */
    @ApiModelProperty("工作简介")
    private String workBrief;
    /**
     * 联系人状态： 1是启用 2是禁用
     */
    @ApiModelProperty("联系人状态： 1是启用 2是禁用")
    private Integer status;
    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;
    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    private Date updateTime;
    /**
     * 表示逻辑删除，1-是删除，0-不删除
     */
    @ApiModelProperty("表示逻辑删除，1-是删除，0-不删除")
    private Integer deleted;

}
