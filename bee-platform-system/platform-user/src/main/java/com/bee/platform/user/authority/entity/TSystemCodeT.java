package com.bee.platform.user.authority.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * <p>
 * 系统码表
 * </p>
 *
 * @author chenjie123123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("t_system_code_t")
public class TSystemCodeT extends Model<TSystemCodeT> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 组id（不唯一）
     */
    private String sysGroupId;
    /**
     * 该组id下面的key(唯一)
     */
    private String sysCode;
    /**
     * 该码表的这个编码对应的值
     */
    private String sysCodeVal;
    /**
     * 该码值的说明x信息
     */
    private String sysCodeDesc;
    /**
     * 是否启用的状态：1->启用，0->禁用
     */
    private String status;
    /**
     * 序号
     */
    private Integer orderNum;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
