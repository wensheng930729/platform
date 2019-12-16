package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 码表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-31
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
public class ErpCode extends Model<ErpCode> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    private String code;
    private String value;
    private String name;
    private String description;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
