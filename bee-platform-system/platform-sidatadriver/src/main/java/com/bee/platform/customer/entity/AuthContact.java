package com.bee.platform.customer.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 *  联系人
 * </p>
 *
 * @author hongchuan.he
 * @since 2019-05-20
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("auth_contact")
public class AuthContact extends Model<AuthContact> {

    private static final long serialVersionUID = 1L;

    /**
     * 联系人主键id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 联系人编号
     */
    private String contactNo;
    /**
     * 二级分类
     */
    private String secondType;
    /**
     * 联系人姓名
     */
    private String name;
    /**
     * 性别
     */
    private String sex;
    /**
     * 电话号码
     */
    private String phone;
    /**
     * 座机
     */
    private String fixtel;
    /**
     * 生日
     */
    private String birthday;
    /**
     * 联系人的邮寄地址
     */
    private String address;
    /**
     * 爱好
     */
    private String hobby;
    /**
     * 联系人星级评定：1是一星 2是二星 3是三星 4是四星 5是五星
     */
    private Integer starLevel;
    /**
     * 工作简介
     */
    private String workBrief;
    /**
     * 联系人状态： 1是启用 2是禁用
     */
    private Integer status;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 表示逻辑删除，1-是删除，0-不删除
     */
    private Integer deleted;
    /**
     * 删除时间
     */
    private Date deletedTime;
    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
