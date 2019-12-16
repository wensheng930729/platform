package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.*;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @notes 学习指南
 * @Author junyang.li
 * @Date 10:54 2019/3/22
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("learn")
public class Learn extends Model<Learn> {

    private static final long serialVersionUID = 1L;

    /**
     * 学习指南文章id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业id
     */
    private Integer orgId;
    /**
     * 用户id
     */
    private Integer userId;
    /**
     * 文章标题
     */
    private String title;
    /**
     * 文章内容
     */
    private String content;
    /**
     * 点击量
     */
    private Integer hits;
    /**
     * 文章发布日期
     */
    private Date createAt;
    /**
     * 文章修改日期
     */
    private Date updateAt;
    /**
     * 类型 使用指南 0 内部培训 1 平台信息 2 规章制度 3 
     */
    private Integer type;
    /**
     * 状态 0 有效 1 无效
     */
    private Integer status;
    /**
     * 部门名称
     */
    private String depName;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
